Babysitter
===

What
---
Babysitter is basic os-process monitor. It will "watch" processes that you run and report when they terminate and will attempt to terminate processes with the erlang process.

This was written to manage [thin processes](http://code.macournoyer.com/thin/) and is being used in [beehive](http://github.com/auser/beehive/).

Quickstart
---
    make && ./start.sh

To start an application, issue a command similar to:

    babysitter:spawn_new([
      {start_command, "thin --port [[PORT]] -R home/app/config.ru start"},
      {cd, "/path/to/app"},
      {image, "app.img"},
      {dirs,  ["/var/lib/gems/1.8", "/usr/bin", "/usr/lib/ruby/"]},
      {env, ["GEM_HOME=/home/.gems"]},
      {variables, [{"[[PORT]]", "5001"}]}
    ], self()).

The spawn_new/2 command takes a proplist and the caller. The proplist can contain the following properties

<table><tr><th>Property</th><th>Description</th></tr>
  <tr><td><tt>start_command</tt></td><td>The command issued to start the os process. This will default to "thin -R config.ru start" if this is not issued</td></tr>
  <tr><td><tt>stop_command</tt></td><td>The command issued to stop the os process. Default: kill -9 [[PID]] where [[PID]] is filled in by babysitter</td></tr>
  <tr><td><tt>cd</tt></td><td>The working directory that the os process will work from</td></tr>
  <tr><td><tt>variables</tt></td><td>The variables that will fill in the start/stop command's templates</td></tr>
</table>

To stop the application, simply pass in the Pid of the process to:

    babysitter:stop_process(Pid)
    
Credit
---
* Credit where credit is due, the basis of the babysitter port-program is based off the brilliant work of [Erlexec](http://code.google.com/p/erlexec/)
* The isolation process is based on [isolate](http://code.google.com/p/isolate/)