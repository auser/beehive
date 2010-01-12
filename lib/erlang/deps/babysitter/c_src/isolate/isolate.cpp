/**
 * isolate -- Isolate a user process by limiting its access to the operating
 * system and the environment.
 *
 * By Chris Palmer <chris@isecpartners.com>.
 *
 * $Id: isolate.cpp 13 2010-01-05 18:45:01Z snackypants $
 *
 * Released under the terms of the GNU General Public License, version 2.
 */


#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <gelf.h>
#ifdef linux
      #include <grp.h>    // For setgroups(2)
#endif
#include <libgen.h>
#include <regex.h>
#include <signal.h>

#include <cstring>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <stdexcept>

using namespace std;

#include "configuration.h"
#include "help.h"
#include "privilege.h"


typedef set<string> string_set;


/* Global variables. */

static string confinement_path;
string image_path;

static uid_t isolator, invoker;
// These have to be global so that clean_up can take no arguments (and hence
// be a suitable atexit handler).
static bool save_isolation_path = false;
unsigned int mdmfs = 0;
static bool x11_authentication_needed = false;


/* Functions. */

/**
 * NOTE: The implementation of this function is amusing.
 *
 * @param n The number to convert to a string.
 *
 * @param base The base in which to represent the number. Can be 8, 10, or
 * 16. Any other value is treated as if it were 10.
 *
 * @return A pointer to a static array of characters representing the number
 * as a string.
 */
static const char * const to_string(long long int n, unsigned char base) {
      static char bfr[32];

      const char * frmt = "%lld";
      if (16 == base) {
            frmt = "%llx";
      } else if (8 == base) {
            frmt = "%llo";
      }

      snprintf(bfr, sizeof(bfr) / sizeof(bfr[0]), frmt, n);
      return bfr;
}


/**
 * @param source The name of the source file.
 * @param destination The name of the destination file.
 *
 * @throws runtime_error If the file could not be copied for any reason.
 */
static void copy_file(const string & source, const string & destination) throw (runtime_error) {
      struct stat stt;
      if (0 == stat(destination.c_str(), &stt)) {
            return;
      }

      FILE *src = fopen(source.c_str(), "rb");
      if (NULL == src) {
             throw runtime_error(string("Could not read ") + source + ": " + strerror(errno));
      }

      FILE *dstntn = fopen(destination.c_str(), "wb");
      if (NULL == dstntn) {
            fclose(src);
            throw runtime_error(string("Could not write ") + destination + ": " + strerror(errno));
      }

      char bfr [4096];
      while (true) {
            size_t r = fread(bfr, 1, sizeof bfr, src);
            if (0 == r || r != fwrite(bfr, 1, r, dstntn)) {
                  break;
            }
      }

      fclose(src);
      if (fclose(dstntn)) {
            throw runtime_error(string("Could not write ") + destination + ": " + strerror(errno));
      }
}


/**
 * @param path The path to check.
 *
 * @returns true if the path is absolute. Note that our definition of
 * "absolute" includes paths beginning with "./", since they are "absolute"
 * relative to a known path. Paths beginning with "../" are not absolute,
 * nor are paths that do not begin with "/" or "./" such as "ls" or
 * "foo.txt".
 */
static bool is_absolute_path(const string & path) throw () {
      return '/' == path[0]
             || ('.' == path[0] && '/' == path[1]);
}


/**

 * @param file The basename of a file to find in one of the directories
 * specified in $PATH. (If $PATH is not available, DEFAULT_PATH is used.)
 *
 * @return A new string giving the full path in which file was found.
 *
 * @throws range_error if file was not found in $PATH/DEFAULT_PATH.
 */
static string which(const string & file) throw (runtime_error) {
      assert(geteuid() != 0 && getegid() != 0);

      if (is_absolute_path(file)) {
            return file;
      }

      string pth = DEFAULT_PATH;
      char *p = getenv("PATH");
      if (p) {
            pth = p;
      }

      string::size_type i = 0;
      string::size_type f = pth.find(':', i);
      do {
            string s = pth.substr(i, f - i) + "/" + file;
            if (0 == access(s.c_str(), X_OK)) {
                  return s;
            }

            i = f + 1;
            f = pth.find(':', i);
      } while (string::npos != f);

      if (! is_absolute_path(file)) {
            throw runtime_error("File not found in $PATH");
      }

      if (0 == access(file.c_str(), X_OK)) {
            return file;
      }

      throw runtime_error("File not found in $PATH");
}


/**
 * @param matchee The string to match against pattern.
 * @param pattern The pattern to test.
 * @param flags The regular expression compilation flags (see regex(3)).
 * (REG_EXTENDED|REG_NOSUB is applied implicitly.)
 *
 * @return true if matchee matches pattern, false otherwise.
 */
static bool matches_pattern(const string & matchee, const char * pattern, int flags)
      throw (logic_error)
{
      regex_t xprsn;
      if (regcomp(&xprsn, pattern, flags|REG_EXTENDED|REG_NOSUB)) {
            throw logic_error("Failed to compile regular expression");
      }

      if (0 == regexec(&xprsn, matchee.c_str(), 1, NULL, 0)) {
            regfree(&xprsn);
            return true;
      }

      regfree(&xprsn);
      return false;
}


/**
 * NOTE: The regular expression allows e.g. "......:0", which is illegal.
 * Our purpose here is to make sure that display is safe to interpolate in a
 * command we send to system(3), not to make sure it is 100% true and good.
 * Safe but illegal/incorrect/wacky display names will cause xauth to fail
 * (e.g. "host not found"). I consider that okay: isolate will give users
 * with bad $DISPLAYs the same results they would have gotten without
 * isolate.
 *
 * @param display The X11 display name to check.
 *
 * @return true if the display name is valid, false otherwise.
 */
static bool is_safe_x11_display(const string & display) throw (logic_error) {
      return matches_pattern(display, "^[.0-9a-z_-]*:[0-9]+\\.?[0-9]*$", REG_ICASE);
}


/**
 * Creates an .Xauthority file in the confinement_path so that the isolatee
 * can talk to the X display (as an (possibly un)trusted client).
 *
 * @param display The X display name.
 * @param trust, The level of trust for the X authorization cookie.
 * @param timeout The timeout for the X authorization.
 * @throws runtime_error if display is not safe, as per is_safe_x11_display,
 * or if the X authorization cannot be made for any reason.
 */
static void x11_authentication(string display, string trust, unsigned int timeout) throw (runtime_error) {
      drop_privilege_temporarily(invoker);

      if (! is_safe_x11_display(display)) {
            throw runtime_error("Invalid display \"" + display + '"');
      }

      const char *hm = getenv("HOME");
      if (NULL == hm) {
            hm = ".";
      }

      string fl = string(hm) + '/' + to_string(isolator, 16) + "-Xauthority";
      string cmnd = XAUTH + " -f " + fl + " generate " + display + " . " + trust + " timeout "
                          + to_string(timeout, 10);

      if (system(cmnd.c_str())) {
            cerr << "Warning: Could not generate X authentication" << endl;
      }

      if (chmod(fl.c_str(), 0644)) {
            throw runtime_error(string("Could not chmod ") + fl + ": " + strerror(errno));
      }

      restore_privilege();

      if (rename(fl.c_str(), (confinement_path + "/.Xauthority").c_str())) {
            copy_file(fl, confinement_path + "/.Xauthority");
            unlink(fl.c_str());
      }

      if (chmod( (confinement_path + "/.Xauthority").c_str(), 0444)) {
            cerr << "Warning: Could not chmod " << confinement_path << "/.Xauthority:"
                 << strerror(errno) << endl;
      }

      drop_privilege_temporarily(isolator);
      string LN = "/bin/ln";
      cmnd = confinement_path + "/tmp/.X11-unix";
      mkdir(cmnd.c_str(), 0777);
      cmnd = LN + " /tmp/.X11-unix/X0 " + confinement_path + "/tmp/.X11-unix/X0";
      system(cmnd.c_str());
      
      restore_privilege();
}


/**
 * @param path The path to create, with successive calls to mkdir(2).
 *
 * @throws runtime_error If any call to mkdir failed.
 */
static void make_path(const string & path) {
      struct stat stt;
      if (0 == stat(path.c_str(), &stt) && S_ISDIR(stt.st_mode)) {
            return;
      }

      string::size_type lngth = path.length();
      for (string::size_type i = 1; i < lngth; ++i) {
            if (lngth - 1 == i || '/' == path[i]) {
                  string p = path.substr(0, i + 1);
                  if (mkdir(p.c_str(), 0750) && EEXIST != errno && EISDIR != errno) {
                        throw runtime_error("Could not create " + p + ": " + strerror(errno));
                  }
            }
      }
}


/**
 * @throws runtime_error If CONFINEMENT_ROOT is not owned by UID and GID
 * 0, if it exists but does not have CONFINEMENT_ROOT_MODE, or if
 * CONFINEMENT_ROOT cannot be created.
 */
static void create_confinement_root() throw (runtime_error) {
      struct stat stt;

      if (0 == stat(CONFINEMENT_ROOT.c_str(), &stt)) {
            if (0 != stt.st_uid || 0 != stt.st_gid) {
                  throw runtime_error(CONFINEMENT_ROOT + " not owned by 0:0. Cannot continue.");
            }

            if (stt.st_mode != CONFINEMENT_ROOT_MODE) {
                  throw runtime_error(CONFINEMENT_ROOT + " is mode " + to_string(stt.st_mode, 8)
                        + "; should be " + to_string(CONFINEMENT_ROOT_MODE, 8));
            }
      }
      else if (ENOENT == errno) {
            /* Make sure we have the right EGID so that the directory has
             * the right group ownership. */
            gid_t egid = getegid();
            setegid(0);

            if (mkdir(CONFINEMENT_ROOT.c_str(), CONFINEMENT_ROOT_MODE)) {
                  throw runtime_error("Could not create " + CONFINEMENT_ROOT);
            }

            setegid(egid);
      }
      else {
            throw runtime_error(CONFINEMENT_ROOT + ": " + strerror(errno));
      }
}


static map<int, string> resources;
static void populate_resources() {
      resources[RLIMIT_AS]      = "RLIMIT_AS (virtual memory size)";
      resources[RLIMIT_CORE]    = "RLIMIT_CORE (core file size)";
      resources[RLIMIT_DATA]    = "RLIMIT_DATA (data segment size)";
      resources[RLIMIT_NOFILE]  = "RLIMIT_NOFILE (number of files)";
      resources[RLIMIT_MEMLOCK] = "RLIMIT_MEMLOCK (locked memory size)";
#ifndef linux
      resources[RLIMIT_SBSIZE]  = "RLIMIT_SBSIZE (socket buffer size)";
#endif
      resources[RLIMIT_NPROC]   = "RLIMIT_NPROC (simultaneous processes)";
      resources[RLIMIT_RSS]     = "RLIMIT_RSS (resident set size)";
      resources[RLIMIT_STACK]   = "RLIMIT_STACK (stack size)";
      resources[RLIMIT_CPU]     = "RLIMIT_CPU (CPU time)";
      resources[RLIMIT_FSIZE]   = "RLIMIT_FSIZE (file size)";
}

/**
 * @param resource The resource to set the limit on. (See setrlimit(2).)
 *
 * @param limit The value -- hard and soft -- of the limit.
 *
 * @throws runtime_error If setrlimit fails.
 */
static void set_resource_limit(const int resource, const rlim_t limit) throw (runtime_error) {
      assert(geteuid() == invoker && getegid() == invoker);

      struct rlimit rlmt = { limit, limit };
      if (setrlimit(resource, &rlmt)) {
            populate_resources();
            throw runtime_error("Could not set resource " + resources[resource]
                  + " to " + to_string(limit, 10) + ": " + strerror(errno));
      }
}


/**
 * Confines the process to confinement_path.
 *
 * @throws runtime_error If chroot(2) or chdir(2) fail.
 */
static void confine() throw (runtime_error) {
      if (chdir(confinement_path.c_str()) || chroot(confinement_path.c_str()) || chdir("/")) {
            throw runtime_error("Could not confine to " + confinement_path + ": "
                  + strerror(errno));
      }
}


/**
 * Sends signal 0 to all processes running as isolator. This function
 * returns only if that failed because of ESRCH -- i.e. if no processes were
 * running as isolator.
 *
 * NOTE: This technique appears not to work on Linux -- kill(-1, 0) always
 * returns 0.
 *
 * NOTE: This function is of only limited usefulness, since there is an
 * inherent race condition.  There is no real way to ensure that nobody is
 * running with the same UID, or to lock other UIDs from spawning processes.
 *
 * @throws runtime_error If it was impossible to fork, or if other
 * processes had been running as isolator.
 */
static void ensure_no_processes() throw (runtime_error) {
#ifndef linux
      int sts = 0;
      errno = 0;
      pid_t pid = fork();

      if (-1 == pid) {
            throw runtime_error("Could not fork!");
      }

      if (0 == pid) {
            drop_privilege_permanently(isolator);

            if (0 == kill(-1, 0)) {
                  _exit(1);
            } else if (ESRCH != errno) {
                  _exit(errno);
            }

            _exit(0);
      }
      else {
            (void) wait(&sts);
      }

      if (0 != WEXITSTATUS(sts)) {
            throw runtime_error(string("Could not ensure that no processes were running as UID ")
                  + to_string(isolator, 10));
      }
#endif
}


/**
 * @return An unpredictable (uid_t is not large enough for me to say
 * "secure") UID.
 *
 * @throws runtime_error If a security assertion cannot be met.
 */
static uid_t random_uid() throw (runtime_error) {
      uid_t u;
      for (unsigned char i = 0; i < 10; i++) {
#ifdef linux
            
            int rndm = open(DEV_RANDOM.c_str(), O_RDONLY);
            if (sizeof(u) != read(rndm, reinterpret_cast<char *>(&u), sizeof(u))) {
                  continue;
            }
            close(rndm);
#else
            u = arc4random();
#endif

            if (u > 0xFFFF) {
              return u;
            }
      }

      throw runtime_error("Could not generate a good random UID after 10 attempts. Bummer!");
}


/**
 * @param file The file to test.
 *
 * @return True if the file's first two bytes are "#!". Advances the file
 * pointer by two bytes.
 */
static bool shell_magic(FILE *file) throw () {
      char bfr[2];
      if (2 != fread(bfr, sizeof(char), 2, file)) {
            return false;
      }

      if ('#' == bfr[0] && '!' == bfr[1]) {
            return true;
      }

      return false;
}


/**
 * @param name A string that might be the name of a code library, e.g.
 * libc.so.7.
 *
 * @return Whether or not the string is plausibly the name of a code
 * library.
 */
static bool names_library(const string & name) throw() {
    return matches_pattern(name, "^lib.+\\.so[.0-9]*$", 0);
}


/**
 * @param elf A pointer to an Elf object (see elf(3)).
 *
 * @return A pair of vectors of strings. The first member is a vector of
 * libraries (e.g. "libc.so.7"), and the second is a vector of search paths
 * (e.g. "/usr/local/gadget/lib").
 *
 * @throws runtime_error If the Elf could not be parsed normally.
 */
static pair<string_set *, string_set *> * dynamic_loads(Elf *elf) throw (runtime_error) {
      assert(geteuid() != 0 && getegid() != 0);

      GElf_Ehdr ehdr;
      if (! gelf_getehdr(elf, &ehdr)) {
            throw runtime_error(string("elf_getehdr failed: ") + elf_errmsg(-1));
      }

      Elf_Scn *scn = elf_nextscn(elf, NULL);
      GElf_Shdr shdr;
      string to_find = ".dynstr";
      while (scn) {
            if (NULL == gelf_getshdr(scn, &shdr)) {
                  throw runtime_error(string("getshdr() failed: ") + elf_errmsg(-1));
            }

            char * nm = elf_strptr(elf, ehdr.e_shstrndx, shdr.sh_name);
            if (NULL == nm) {
                  throw runtime_error(string("elf_strptr() failed: ") + elf_errmsg(-1));
            }

            if (to_find == nm) {
                  break;
            }

            scn = elf_nextscn(elf, scn);
      }

      Elf_Data *data = NULL;
      size_t n = 0;
      string_set *lbrrs = new string_set(), *pths = new string_set();

      pths->insert("/lib");
      pths->insert("/usr/lib");
      pths->insert("/usr/local/lib");

      while (n < shdr.sh_size && (data = elf_getdata(scn, data)) ) {
            char *bfr = static_cast<char *>(data->d_buf);
            char *p = bfr + 1;
            while (p < bfr + data->d_size) {
                  if (names_library(p)) {
                        lbrrs->insert(p);
                        if (string(p).substr(0, 9) == "libX11.so") {
                              x11_authentication_needed = true;
                        }
                  } else if ('/' == *p) {
                        pths->insert(p);
                  }

                  size_t lngth = strlen(p) + 1;
                  n += lngth;
                  p += lngth;
            }
      }

      return new pair<string_set *, string_set *>(lbrrs, pths);
}


static string_set already_copied;

/**
 * @param image The path to the executable image file.
 *
 * @return The number of dependencies copied into confinement_path.  XXX:
 * This allows a malicious isolatee to increase the number of files it can
 * open. However, I think it is all under the control of the loader?
 *
 * @throws runtime_error if the image could not be parsed. Does not throw
 * if a dependency could not be copied.
 */
static rlim_t copy_dependencies(const string & image) throw (runtime_error) {
      if (EV_NONE == elf_version(EV_CURRENT)) {
            throw runtime_error(string("ELF library initialization failed: ") + elf_errmsg(-1));
      }

      int fl = open(image.c_str(), O_RDONLY);
      if (-1 == fl) {
            throw runtime_error(string("Could not open ") + image + ": " + strerror(errno));
      }

      Elf *elf = elf_begin(fl, ELF_C_READ, NULL);
      if (NULL == elf) {
            throw runtime_error(string("elf_begin failed: ") + elf_errmsg(-1));
      }

      if (ELF_K_ELF != elf_kind(elf)) {
            throw runtime_error(image + " is not an ELF object.");
      }

      pair<string_set *, string_set *> *r = dynamic_loads(elf);
      string_set lds = *r->first;
      rlim_t lmt_fls = 0;

      for (string_set::iterator ld = lds.begin(); ld != lds.end(); ++ld) {
            string_set pths = *r->second;
            bool fnd = false;

            for (string_set::iterator pth = pths.begin(); pth != pths.end(); ++pth) {
                  string src = *pth + '/' + *ld;
                  if (already_copied.count(src)) {
                        fnd = true;
                        continue;
                  }
                  string dstntn = confinement_path + src;

                  try {
#ifdef linux
                        make_path(dirname(strdup(dstntn.c_str())));
#else
                        make_path(dirname(dstntn.c_str()));
#endif
                        copy_file(src, dstntn);
                        lmt_fls += 1;
                        fnd = true;
                        already_copied.insert(src);
                        lmt_fls += copy_dependencies(src);    // FIXME: Grossly inefficient.
                        break;
                  } catch (runtime_error e) { }
            }

            if (! fnd) {
                  cerr << "Could not find library " << *ld << endl;
            }
      }

      elf_end(elf);
      close(fl);
      return lmt_fls;
}


/**
 * Prints the resource usage of the child process to cerr.
 */
static void print_rusage() throw () {
      struct rusage usg;
      if (getrusage(RUSAGE_CHILDREN, &usg)) {
            cerr << "Could not get child's resource usage: " << strerror(errno) << endl;
            return;
      }

      cerr << "User time: " << usg.ru_utime.tv_sec << "." << setfill('0') << setw(6)
           << usg.ru_utime.tv_usec << endl
           << "System time: " << usg.ru_stime.tv_sec << "." << setfill('0') << setw(6)
           << usg.ru_stime.tv_usec << endl
           << "Unshared data size: " << usg.ru_idrss << endl
           << "Unshared stack size: " << usg.ru_isrss << endl;
}


/**
 * @param resource The resource for which to find the limit. See
 * getrlimit(2).
 *
 * @return The current process' maximum limit for the given resource.
 */
static rlim_t get_resource_limit(int resource) throw () {
      struct rlimit rlmt;
      if (getrlimit(resource, &rlmt)) {
            throw runtime_error(string("getrlimit: ") + strerror(errno));
      }
      return rlmt.rlim_max;
}


/**
 * For each support path, recursively copies it into confinement_path by
 * calling cp(1) through system(3).
 *
 * @param support_paths The paths to the support directories.
 *
 * @throws runtime_error if the cp command fails for any reason.
 */
static void copy_support_paths(const string_set & support_paths) throw (runtime_error) {
      for (string_set::const_iterator i = support_paths.begin(); i != support_paths.end(); ++i) {
            string src = *i;
            string wildcard ("*");
            size_t found;
            
            found = src.find(wildcard);
            
            // Make the paths
#ifdef linux
            string dstntn_pth = confinement_path + dirname(strdup(src.c_str()));
#else
            string dstntn_pth = confinement_path + dirname(src.c_str());
#endif      

            if(found != string::npos)
            {
              // We found a wildcard, we'll use a command instead
              make_path(dstntn_pth.c_str());
              string cmnd = CP + " -RL" + " " + src + " " + dstntn_pth + "";
              if (system(cmnd.c_str())) {
                    throw runtime_error("Could not copy " + src + " into " + confinement_path);
              }
            } else {
              struct stat stt;
              if (stat(src.c_str(), &stt)) {
                    throw runtime_error(src + ": " + strerror(errno));
              }

              make_path(dstntn_pth.c_str());

              // If it's just a file, copy it directly and continue.
              if (S_ISREG(stt.st_mode)) {
                    copy_file(src, confinement_path + src);
                    continue;
              }

              string cmnd = CP + " -RL" + " \"" + src + "\" \"" + dstntn_pth + "\"";
              if (system(cmnd.c_str())) {
                    throw runtime_error("Could not copy " + src + " into " + confinement_path);
              }
            }
      }
}

/**
 * Performs necessary filesystem clean-up after the isolatee exits.
 */
static void clean_up() throw () {
      string pth;

#ifndef linux
      pth = confinement_path + "/dev";
      if (system( (UMOUNT + ' ' + pth).c_str() )) {
            cerr << "WARNING: Could not unmount " << pth << endl;
      }
#endif
      
      if(!image_path.empty())
      {
        cerr << "Current user " << getuid() << ":" << getgid() << " or " << isolator << ":" << isolator << endl;
        if (system( (UMOUNT + ' ' + confinement_path + "/home").c_str() )) {
          cerr << "WARNING: Could not unmount " << image_path << endl;
          cerr << "Current user " << getuid() << ":" << getgid() << endl;
          sleep(5);
        }
      }

      if (save_isolation_path) {
            string cmnd = CHOWN + " -R " + to_string(getuid(), 10) + ':' + to_string(getgid(), 10)
                         + ' ' + confinement_path;
            if (system(cmnd.c_str())) {
                  cerr << "WARNING: Could not chown " << confinement_path << " to "
                       << getuid() << ":" << getgid() << endl;
            } else {
                  cerr << "Chowned " << confinement_path << " to "
                       << getuid() << ":" << getgid() << endl;
            }
      } else if (mdmfs > 0) {
            if (system( (UMOUNT + ' ' + confinement_path).c_str() )) {
                  cerr << "WARNING: Could not unmount " << confinement_path << endl;
            }

            if (system( (RMDIR + ' ' + confinement_path).c_str() )) {
                  cerr << "WARNING: Could not delete " << confinement_path << endl;
            }
      } else {
            if (system( (RM + " -rf " + confinement_path).c_str() )) {
                  cerr << "WARNING: Could not delete " << confinement_path << endl;
            }
      }
}

void signal_handler(int received) {
      cerr << "Received signal " << received << endl;
      ensure_no_processes();
      clean_up();
      cerr << endl;
      cout << endl;
      _exit(1);
}


/**
 * Installs the signal handlers and the atexit handler to clean up the
 * isolation environment when the parent exits.
 */
void install_handlers() {
      atexit(clean_up);
#ifdef linux
      #define signal bsd_signal
#endif
      signal(SIGHUP, signal_handler);
      signal(SIGINT, signal_handler);
      signal(SIGQUIT, signal_handler);
      signal(SIGILL, signal_handler);
      signal(SIGTRAP, signal_handler);
      signal(SIGABRT, signal_handler);
      signal(SIGFPE, signal_handler);
      signal(SIGBUS, signal_handler);
      signal(SIGSEGV, signal_handler);
      signal(SIGSYS, signal_handler);
      signal(SIGPIPE, signal_handler);
      signal(SIGALRM, signal_handler);
      signal(SIGTERM, signal_handler);
      signal(SIGTTIN, signal_handler);
      signal(SIGTTOU, signal_handler);
      signal(SIGXCPU, signal_handler);
      signal(SIGXFSZ, signal_handler);
      signal(SIGVTALRM, signal_handler);
      signal(SIGPROF, signal_handler);
      signal(SIGUSR1, signal_handler);
      signal(SIGUSR2, signal_handler);
#ifndef linux
      signal(SIGTHR, signal_handler);
      signal(SIGEMT, signal_handler);
#endif
}

int main(int argc, char *argv[]) {

      /* Set default configuration. */

      rlim_t
            lmt_all = min(DEFAULT_MEMORY_LIMIT, get_resource_limit(RLIMIT_AS)),
            lmt_cr = min(DEFAULT_MEMORY_LIMIT, get_resource_limit(RLIMIT_CORE)),
            lmt_dta = min(DEFAULT_MEMORY_LIMIT, get_resource_limit(RLIMIT_DATA)),
            lmt_fls = min(static_cast<rlim_t>(3), get_resource_limit(RLIMIT_NOFILE)),
            lmt_fl_sz = min(DEFAULT_MEMORY_LIMIT, get_resource_limit(RLIMIT_FSIZE)),
            lmt_mlck = min(static_cast<rlim_t>(0x200000), get_resource_limit(RLIMIT_MEMLOCK)),
#ifndef linux
            lmt_ntwrk = min(static_cast<rlim_t>(0x200000), get_resource_limit(RLIMIT_SBSIZE)),
#endif
            lmt_prcs = min(static_cast<rlim_t>(0), get_resource_limit(RLIMIT_NPROC)),
            lmt_rss = min(DEFAULT_MEMORY_LIMIT, get_resource_limit(RLIMIT_RSS)),
            lmt_stck = min(DEFAULT_MEMORY_LIMIT, get_resource_limit(RLIMIT_STACK)),
            lmt_tm = min(static_cast<rlim_t>(10), get_resource_limit(RLIMIT_CPU))
            ;
      string cookie_trust = "untrusted";
      bool vrbs = false;

      string_set sprt_pths;
      string confinement_root = DEFAULT_CONFINEMENT_ROOT;
      string skel_dir;
      
      int curr_env_vars;
      /* Set default environment variables */
      /* We need to set this so that the loader can find libs in /usr/local/lib. */
      const char* default_env_vars[] = {
        "LD_LIBRARY_PATH=/lib;/usr/lib;/usr/local/lib", 
        "HOME=/home"
      };
      const int max_env_vars = 1000;
      char *env_vars[max_env_vars];
      
      memcpy(env_vars, default_env_vars, (min((int)max_env_vars, (int)sizeof(default_env_vars)) * sizeof(char *)));
      curr_env_vars = sizeof(default_env_vars) / sizeof(char *);
      
      /* Parse command line. */

      char c;
      while (-1 != (c = getopt(argc, argv, "a:b:c:C:d:D:e:f:i:hm:M:n:p:r:s:St:Tvz:"))) {
            switch (c) {
            case 'a':
                  lmt_all = strtoll(optarg, NULL, 0);
                  break;
            case 'b':
                  skel_dir = string(optarg);
                  break;
            case 'c':
                  lmt_cr = strtoll(optarg, NULL, 0);
                  break;
            case 'C':
                  confinement_root = string(optarg);
                  break;
            case 'd':
                  lmt_dta = strtoll(optarg, NULL, 0);
                  break;
            case 'D':
                  sprt_pths.insert(string(optarg));
                  break;
            case 'e':
                  if (curr_env_vars <= max_env_vars)
                    env_vars[curr_env_vars++] = strdup(optarg);
                    cout << "Added env " << env_vars[curr_env_vars-1] << endl;
                  break;
            case 'i':
                  image_path = string(optarg);
                  break;
            case 'f':
                  lmt_fls = strtoll(optarg, NULL, 0);
                  break;
            case 'm':
                  lmt_mlck = strtoll(optarg, NULL, 0);
                  break;
            case 'M':
                  mdmfs = strtoll(optarg, NULL, 0);
                  break;
#ifndef linux
            case 'n':
                  lmt_ntwrk = strtoll(optarg, NULL, 0);
                  break;
#endif
            case 'p':
                  lmt_prcs = strtoll(optarg, NULL, 0);
                  break;
            case 'r':
                  lmt_rss = strtoll(optarg, NULL, 0);
                  break;
            case 's':
                  lmt_stck = strtoll(optarg, NULL, 0);
                  break;
            case 'S':
                  save_isolation_path = true;
                  break;
            case 't':
                  lmt_tm = strtoll(optarg, NULL, 0);
                  break;
            case 'T':
                  cookie_trust = "trusted";
                  break;
            case 'v':
                  vrbs = true;
                  break;
            case 'z':
                  lmt_fl_sz = strtoll(optarg, NULL, 0);
                  break;
            case 'h':
                  // FALLTHRU
            default:
                  cerr << HELP_MESSAGE;
                  return 1;
            }
      }
      
      if(!confinement_root.empty())
        CONFINEMENT_ROOT = confinement_root;
      
      argc -= optind;
      argv += optind;

      if (NULL == argv || NULL == *argv) {
            cerr << HELP_MESSAGE;
            return 1;
      }

      // TODO: Fix this.
      if (mdmfs && save_isolation_path) {
            cerr << "Unfortunately, the -M and -S options cannot be used together." << endl;
            return 1;
      }

      vrbs && cerr << "Resource limits:" << endl
                   << "      RLIMIT_AS           0x" << hex << lmt_all << " bytes" << endl
                   << "      RLIMIT_CORE         0x" << hex << lmt_cr << " bytes" << endl
                   << "      RLIMIT_DATA         0x" << hex << lmt_dta << " bytes" << endl
                   << "      RLIMIT_NOFILE       " << dec << lmt_fls << " files" << endl
                   << "      RLIMIT_FSIZE        0x" << hex << lmt_fl_sz << " bytes" << endl
                   << "      RLIMIT_MEMLOCK      0x" << hex << lmt_mlck << " bytes" << endl
#ifndef linux
                   << "      RLIMIT_SBSIZE       0x" << hex << lmt_ntwrk << " bytes" << endl
#endif
                   << "      RLIMIT_NPROC        " << dec << lmt_prcs << " processes" << endl
                   << "      RLIMIT_RSS          0x" << hex << lmt_rss << " bytes" << endl
                   << "      RLIMIT_STACK        0x" << hex << lmt_stck << " bytes" << endl
                   << "      RLIMIT_CPU          " << dec << lmt_tm << " seconds" << endl
                   ;


      /* Create a random UID for the isolator. */
      // TODO: If invoker is root, allow them to specify a UID?
      
      isolator = random_uid();

      invoker = getuid();

      /* Ensure the confinement root exists and is correct. */

      create_confinement_root();


      /* Create the confinement directory. */

      confinement_path = CONFINEMENT_ROOT + '/' + to_string(isolator, 16);
      if (mkdir(confinement_path.c_str(), CONFINEMENT_ROOT_MODE)) {
            throw runtime_error("Could not create " + confinement_path + ":" + strerror(errno));
      }

      string cmnd;
#ifndef linux
      if (mdmfs) {
            cmnd = MDMFS + " -s " + to_string(mdmfs, 10) + "m md " + confinement_path;
            if (system(cmnd.c_str())) {
                  throw runtime_error("Unable to create/mount temporary file system");
            }
      }
#endif

      if (chown(confinement_path.c_str(), isolator, isolator)) {
            throw runtime_error("Could not create " + confinement_path + ":" + strerror(errno));
      }


#ifndef linux
      /* Mount a devfs in the confinement directory. */

      string devfs_pth = confinement_path + "/dev";
      if (mkdir(devfs_pth.c_str(), 0755)) {
            throw runtime_error("Could not create " + devfs_pth + ": " + strerror(errno));
      }

      cmnd = MOUNT + " -t devfs devfs " + confinement_path + "/dev";
      if (system(cmnd.c_str())) {
            throw runtime_error("Could not mount devfs on " + devfs_pth);
      }
#endif

      if (!image_path.empty())
      {
        string at = confinement_path + "/home";
        
        make_path(at);
        cmnd = MOUNT + " -o ro -o loop " + image_path + " " + at;
        
        if (system(cmnd.c_str())) {
              throw runtime_error("Could not mount "+ image_path);
        }
      }

      /* Fork to do the isolation. */

      pid_t chld = fork();
      if (-1 == chld) {
            throw runtime_error("Could not fork");
      }

      int sts = 0;
      if (0 == chld) {
        drop_privilege_temporarily(isolator);
                
        string fl_pth = which(*argv);
        string pth = confinement_path
                   //+ ('/' == confinement_path[confinement_path.length() - 1] ? "" : "/")
                   + fl_pth;
            /** 
             * Copy the necessary files into the confinement directory. 
             * If the user provided a skeleton directory, then we can skip
             * all of the smart lookups and just use that directory
             * otherwise, track down the support files and build the 
             * confinement_root
             **/
            if(! skel_dir.empty() )
            {
              string cmnd = CP + " -RL" + " " + skel_dir + "/* " + confinement_path + "";
              vrbs && cout << "Using skeleton directory " << skel_dir << " with " << cmnd << endl;
              
              if (system(cmnd.c_str())) {
                cerr << "WARNING: Could not build from the skeleton directory" << endl;
              }
              
            } else {
              string_set pths;
              pths.insert("/tmp");
              pths.insert("/bin");
              pths.insert("/lib");
              pths.insert("/libexec");
              pths.insert("/etc");
              for (string_set::iterator i = pths.begin(); i != pths.end(); ++i) {
                    string p = confinement_path + *i;
                    if (mkdir(p.c_str(), CONFINEMENT_ROOT_MODE)) {
                        cerr << "Could not build confinement directory: " << strerror(errno) << endl;
                        _exit(1);
                    }
              }
              vrbs && cerr << "Built confinement directory " << confinement_path << endl << endl;


              /* Copy the executable image into the confinement directory. */
#ifdef linux
              make_path(dirname(strdup(pth.c_str())));
#else
              make_path(dirname(pth.c_str()));
#endif
              copy_file(fl_pth, pth);

              if (chmod(pth.c_str(), 0755)) {
                    cerr << "Could not make " << pth << " executable: " << strerror(errno) << endl;
                    _exit(1);
              }
              
              /* Add a copy of the user's /etc/passwd */
              string etc_pass_file = string(confinement_path + "/etc/passwd");
              ofstream etc_pass(etc_pass_file.c_str());
              etc_pass << "me:x:" << isolator << ":" << isolator << ":me:/home:bin/bash" ;
              etc_pass << flush;
              etc_pass.close();
              
              /* Add default /etc/hosts */
              string hosts_file_path = string(confinement_path + "/etc/hosts");
              ofstream hosts_file(hosts_file_path.c_str());
              hosts_file << "127.0.0.1    localhost" ;
              hosts_file << flush;
              hosts_file.close();

              /* Copy the support directories into the isolation environment. */
              copy_support_paths(sprt_pths);

              pth = confinement_path + '/' + LD_ELF_SO_PATH;
              copy_file(LD_ELF_SO_PATH, pth);
#ifndef linux
              make_path(confinement_path + dirname(strdup(TERMCAP.c_str())));
              copy_file(TERMCAP, confinement_path + TERMCAP);
#endif

              copy_file(RESOLV_CONF, confinement_path + RESOLV_CONF);

#ifdef has_glibc 
              copy_file(NSSWITCH_CONF, confinement_path + NSSWITCH_CONF);
              copy_file(NSS_COMPAT, confinement_path + NSS_COMPAT);
              copy_file(NSS_DNS, confinement_path + NSS_DNS);
              copy_file(NSS_FILES, confinement_path + NSS_FILES);
              copy_file(LIBRESOLV, confinement_path + LIBRESOLV);
#endif
            
              /** 
               * Work with the executable that the user requested to run
               **/
              if (chmod(pth.c_str(), 0755)) {
                    cerr << "Could not make " << pth << " executable: " << strerror(errno) << endl;
                    _exit(1);
              }


              /* Find out if the requested program is a script, and copy in
               * the script stuff. */

              FILE *fl = fopen(fl_pth.c_str(), "rb");
              if (shell_magic(fl)) {
                    char pth[PATH_MAX];
                    size_t r = fread(pth, sizeof(char), PATH_MAX, fl);
                    pth[r] = '\0';

                    // Trim whitespace at the beginning and end of the
                    // interpreter path.
                    char *p = pth;
                    while (' ' == *p) {
                          p++;
                    }
                    char *q = strchr(p, '\n');
                    if (NULL == q) {
                          q = strchr(p, ' ');
                    }
                    *q = '\0';

                    string cp = p;

#ifdef linux
                    string pth2 = confinement_path + dirname(strdup(cp.c_str()));
#else
                    string pth2 = confinement_path + dirname(cp.c_str());
#endif
                    make_path(pth2);
                    pth2 = confinement_path + p;
                    copy_file(cp, pth2);
                    chmod(pth2.c_str(), 0755);

                    lmt_fls += copy_dependencies(cp) + 5;
              } else {
                    /* Figure out what the requested program links to, and copy
                     * those things into the chroot environment. */

                    lmt_fls += copy_dependencies(fl_pth);
              }
              fclose(fl);

              restore_privilege();

              /* Generate the X authorization. */
              if (x11_authentication_needed) {
                    char * dsply = getenv("DISPLAY");
                    if (dsply) {
                          x11_authentication(dsply, cookie_trust, lmt_tm);
                    }
               }
            }

            /* Do the final sandboxing. */
            restore_privilege();
            ensure_no_processes();
            confine();


            /* Set the resource limits. */
            drop_privilege_temporarily(invoker);
            set_resource_limit(RLIMIT_AS, lmt_all);
            set_resource_limit(RLIMIT_CORE, lmt_cr);
            set_resource_limit(RLIMIT_DATA, lmt_dta);
            //set_resource_limit(RLIMIT_NOFILE, lmt_fls);
            set_resource_limit(RLIMIT_FSIZE, lmt_fl_sz);
            set_resource_limit(RLIMIT_MEMLOCK, lmt_mlck);
#ifndef linux
            set_resource_limit(RLIMIT_SBSIZE, lmt_ntwrk);
#endif
#ifdef linux
            /* On Linux, setresuid(2) can fail, EAGAIN, because there
             * weren't enough processes. Kinky! */
            set_resource_limit(RLIMIT_NPROC, lmt_prcs + 1);
#else
            set_resource_limit(RLIMIT_NPROC, lmt_prcs);
#endif
            set_resource_limit(RLIMIT_RSS, lmt_rss);
            set_resource_limit(RLIMIT_STACK, lmt_stck);
            set_resource_limit(RLIMIT_CPU, lmt_tm);
            restore_privilege();

            /* Become the isolator, irrevocably. */
            drop_privilege_permanently(isolator);

            /* Set the resource limits to their new minima. Previously,
             * they were set to root's minima (at the top of main), but we
             * do this again here with the new, low-privilege values. */
            lmt_all = min(lmt_all, get_resource_limit(RLIMIT_AS));
            lmt_cr = min(lmt_cr, get_resource_limit(RLIMIT_CORE));
            lmt_dta = min(lmt_dta, get_resource_limit(RLIMIT_DATA));
            lmt_fls = min(lmt_fls, get_resource_limit(RLIMIT_NOFILE));
            lmt_fl_sz = min(lmt_fl_sz, get_resource_limit(RLIMIT_FSIZE));
            lmt_mlck = min(lmt_mlck, get_resource_limit(RLIMIT_MEMLOCK));
#ifndef linux
            lmt_ntwrk = min(lmt_ntwrk, get_resource_limit(RLIMIT_SBSIZE));
#endif
            lmt_prcs = min(lmt_prcs, get_resource_limit(RLIMIT_NPROC));
            lmt_rss = min(lmt_rss, get_resource_limit(RLIMIT_RSS));
            lmt_stck = min(lmt_stck, get_resource_limit(RLIMIT_STACK));
            lmt_tm = min(lmt_tm, get_resource_limit(RLIMIT_CPU));


            /* We are ready for launch. */
            execve(fl_pth.c_str(), argv, env_vars);
            cerr << "Could not execute " << *argv << ": " << strerror(errno) << endl;
            _exit(errno);
      }
      else {
            install_handlers();

            (void) wait(&sts);

            if (vrbs) {
                  cerr << endl << "Resource usage statistics for \"";
                  for (int i = 0; i < argc; i++) {
                        cerr << argv[i] << (argc - 1 == i ? "" : " ");
                  }
                  cerr << "\": " << endl;
                  print_rusage();
            }
      }

      return WEXITSTATUS(sts);
}

