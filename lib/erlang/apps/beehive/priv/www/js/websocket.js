var message_queue = []

// I adapted this code from a Jetty Server example.
if (!window.WebSocket)
  alert("WebSocket not supported by this browser");

  var client = {
    connect: function(){
      this._ws=new WebSocket("ws://0.0.0.0:4998");
      this._ws.onopen=this._onopen;
      this._ws.onmessage=this._onmessage;
      this._ws.onclose=this._onclose;
     },

     _onopen: function(){
       client._send('client-connected');
     },

     _send: function(message){
       if (this._ws)
        this._ws.send(message);
      },

     send_to_socket: function(text) {
        if (text != null && text.length>0 )
          client._send(text);
      },

      _onmessage: function(m) {
        if (m.data){

          var text = m.data; 
          // var msg=$('msgs');
          // var spanText = document.createElement('span');
          // spanText.className='text';
          // spanText.innerHTML=text;
          // var lineBreak = document.createElement('br');
          // msg.appendChild(spanText);
          // msg.appendChild(lineBreak);
          // msg.scrollTop = msg.scrollHeight - msg.clientHeight;
   				message_queue.push(text)
        }
      },

      _onclose: function(m) {
        this._ws=null;
      }


  };

function last_socket_message() {
	alert(message_queue.shift());
};

(function($) {
 client.connect(); return false;
})(jQuery);

// Get an Element
function $() { return document.getElementById(arguments[0]); }
// Get the value of an Element
function $F() { return document.getElementById(arguments[0]).value; }