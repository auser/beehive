{application, beehive, [
        {description, "Beehive"},
        {vsn, "0.0.1"},
        {modules, [
                
                % beehive
                beehive_app, beehive_sup, app_discovery, beehive_logger,
                app_registry_srv,
                
                % gen_cluster
                gen_cluster,

                % mochiweb
                mochifmt,mochifmt_records,mochifmt_std,mochihex,mochijson,mochijson2,mochinum,
                mochiweb,mochiweb_app,mochiweb_charref,mochiweb_cookies,mochiweb_echo,
                mochiweb_headers,mochiweb_html,mochiweb_http,mochiweb_multipart,
                mochiweb_request,mochiweb_response,mochiweb_skel,mochiweb_socket_server,
                mochiweb_sup,mochiweb_util
            ]},
        {env, [
          {port, 8643}
        ]},

        {registered, [beehive]},
        {applications, [kernel, stdlib, sasl]},
        {included_applications, []},
        {start_phases, [{go,[]}]},
        {mod, {application_starter,[beehive,[]]}}
]}.
