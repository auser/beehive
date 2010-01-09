% Example app template that will be used to start a bee
{start_command, "[[THIN_APP]] -R home/app/config.ru --log tmp/[[APP_NAME]].log --port [[PORT]] start"}.
{env, ["GEM_HOME=/home/.gems", "LOCAL_PORT=[[PORT]]"]}.
{dirs, ["/var/lib/gems/1.8", "/usr/bin", "/usr/lib/ruby/"]}.
