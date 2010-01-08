/**
 * Adapted from Chen, et al. "Setuid Demystified", section 8.2.2.
 *
 * This implementation copyright 2009 by Chris Palmer. Released under the
 * terms of the GNU General Public License, version 2.
 */

#include <errno.h>
#include <stdio.h>

#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string>

using namespace std;

const static bool DEBUG = false;


/**
 * Changes the process' UID in a way that allows the caller to regain
 * privilege later.
 *
 * @param new_uid The new UID to assume.
 *
 * @throws runtime_error If any of the UID-changing system calls fail.
 */
void drop_privilege_temporarily(uid_t new_uid) throw (runtime_error) {
      if (setresgid(-1, new_uid, getegid())
          || setresuid(-1, new_uid, geteuid())
          || getegid() != new_uid
          || geteuid() != new_uid )
      {
            throw runtime_error(string("Could not drop privilege: ") + strerror(errno));
      }

      if (DEBUG) {
            cerr << "dropped privilege temporarily: " << geteuid() << ":" << getegid() << endl;
      }
}


/**
 * Changes the process' UID in a way that does NOT allow the caller to
 * regain privilege later.
 *
 * @param new_uid The new UID to assume.
 *
 * @throws runtime_error If any of the UID-changing system calls fail.
 */
void drop_privilege_permanently(uid_t new_uid) throw (runtime_error) {
      uid_t ruid, euid, suid;
      gid_t rgid, egid, sgid;

      if (setresgid(new_uid, new_uid, new_uid)
          || setresuid(new_uid, new_uid, new_uid)
          || getresgid(&rgid, &egid, &sgid)
          || getresuid(&ruid, &euid, &suid)
          || rgid != new_uid || egid != new_uid || sgid != new_uid
          || ruid != new_uid || euid != new_uid || suid != new_uid
          || getegid() != new_uid || geteuid() != new_uid )
      {
            throw runtime_error(string("Could not drop privilege: ") + strerror(errno));
      }

      if (DEBUG) {
            cerr << "dropped privilege permanently: " << geteuid() << ":" << getegid() << endl;
      }
}


/**
 * Restores previously dropped privilege.
 *
 * @throws runtime_error If any of the UID-changing system calls fail.
 */
void restore_privilege() throw (runtime_error) {
      uid_t ruid, euid, suid;
      gid_t rgid, egid, sgid;

      if (getresgid(&rgid, &egid, &sgid)
          || getresuid(&ruid, &euid, &suid)
          || setresuid(-1, suid, -1)
          || setresgid(-1, sgid, -1)
          || geteuid() != suid || getegid() != sgid )
      {
            throw runtime_error(string("Could not restore privilege: ") + strerror(errno));
      }

      if (DEBUG) {
            cerr << "saved uid: " << suid << endl;
            cerr << "restored privilege: " << geteuid() << ":" << getegid() << endl;
      }
}

