{application, beehive, [
        {description, "Beehive Client"},
        {vsn, "0.0.1"},
        {modules, [
                
                % beehive
                beehive_client, beehive_client_app, beehive_client_sup,

                % mochiweb
                mochifmt,mochifmt_records,mochifmt_std,mochihex,mochijson,mochijson2,mochinum,
                mochiweb,mochiweb_app,mochiweb_charref,mochiweb_cookies,mochiweb_echo,
                mochiweb_headers,mochiweb_html,mochiweb_http,mochiweb_multipart,
                mochiweb_request,mochiweb_response,mochiweb_skel,mochiweb_socket_server,
                mochiweb_sup,mochiweb_util
            ]},
        {env, [
          {external_app, "thin start"}
        ]},

        {registered, [beehive_client_app]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {beehive_client_app,[]}},
        {included_applications, []}
]}.
