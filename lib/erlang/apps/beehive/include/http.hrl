-define (JSON_ENCODE(V), mochijson2:encode(V)).
-define (JSONIFY (V), web_utils:to_json(V)).

% Base page
-define (HOME_HTML, "<html>
  <head><title>Beehive</title></head>
  <body>
    <h1>Beehive homepage.</h1>
  </body></html>").

-define (CONTENT_HTML (Cont), io_lib:format("<html>
  <head><title>Beehive</title></head>
  <body id='beehive'>
    <div class='content'>
      <h1>Beehive</h1>
      <p>~s</p>
    </div>
  </body></html>", [Cont])).

-define (APP_ERROR (Code, Msg), io_lib:format("HTTP/1.1 ~p Internal Server Error\r\n\r\n<html>
  <head><title>Uh oh! We made a boo boo</title></head>
  <body>
    <h1>Beehive Error</h1>
    <p>~s</p>
  </body></html>", [Code, Msg])).

% Error page
-define (ERROR_HTML (App), io_lib:format("<html>
  <head><title>Error - ~s</title></head>
  <body>
    <h1>Beehive Error | There was an error</h1>
    <p>Check the url again to make sure you typed it correctly</p>
  </body></html>", [App])).

-define (NOT_FOUND_HTML (App), io_lib:format("<html>
  <head><title>Error - ~s Not found</title></head>
  <body>
    <h1>Beehive Error | The site you have requested was not found.</h1>
    <p>Check the url again to make sure you typed it correctly</p>
  </body></html>", [App])).
