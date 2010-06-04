
// I adapted this code from a Jetty Server example.
if (!window.WebSocket)
    alert("WebSocket not supported by this browser");

// Get an Element
function $() { return document.getElementById(arguments[0]); }
// Get the value of an Element
function $F() { return document.getElementById(arguments[0]).value; }

var client = {
   connect: function(){
     this._ws=new WebSocket("ws://localhost:4997");
     this._ws.onopen=this._onopen;
     this._ws.onmessage=this._onmessage;
     this._ws.onclose=this._onclose;
   },

   _onopen: function(){
      $('connect').className='hidden';
      $('connected').className='';
      $('phrase').focus();
      client._send('client-connected');
   },

   _send: function(message){
       if (this._ws)
        this._ws.send(message);
    },
   
   chat: function(text) {
      if (text != null && text.length>0 )
        client._send(text);
    },
    
    _onmessage: function(m) {
      if (m.data){
        var text = m.data; 
        var msg=$('msgs');
        var spanText = document.createElement('span');
        spanText.className='text';
        spanText.innerHTML=text;
        var lineBreak = document.createElement('br');
        msg.appendChild(spanText);
        msg.appendChild(lineBreak);
        msg.scrollTop = msg.scrollHeight - msg.clientHeight;   
      }
    },
    
    _onclose: function(m) {
      this._ws=null;
      $('connect').className='';
      $('connected').className='hidden';
      $('msg').innerHTML='';
    }

   
};