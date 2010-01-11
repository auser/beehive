% Example app template that will be used to start a bee
{start_command, "/usr/bin/thin -- -R home/app/config.ru --log tmp/[[APP_NAME]].log --port [[PORT]] start"}.
{env, ["GEM_HOME=/home/.gems"]}.
{dirs, ["/var/lib/gems/1.8", "/usr/bin", "/usr/lib/ruby/"]}.
