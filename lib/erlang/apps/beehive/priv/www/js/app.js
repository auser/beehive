console.log("sup yo");

(function($) {
  
  $.ajax({
     type: "GET",
     url: "/apps",
     dataType: "json",
     success: function(msg){
       console.log( msg );
     }
   });
  
  var app = $.sammy(function() {
    this.element_selector = '#main';    
    this.use(Sammy.Template);
    this.use(Sammy.Session);

    this.before(function() {
      // load the items
      var context = this;

    });

    this.get('#/', function(context) {
      context.app.swap('');
      
    });
    
  });
  
  $(function() {
    app.run('#/');
  });
  
})(jQuery);