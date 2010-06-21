(function($) {
    
  var app = $.sammy(function() {
    this.element_selector = '#main';    
    this.use(Sammy.Haml);

    this.before(function() {
      var context = this;
    });
    
    this.helpers({
      get_page: function(url, context, model) {
        $.ajax({
          type: "GET",
          async: false,
          url: url,
          dataType: "json",
          success: function(data){
            console.log(data);
            context[model] = data[model];
          }
        });
      }
    });

		this.get('#/', function(context) {
			this.redirect('#/overview');
		});
		
		this.get('#/overview', function(context) {
      context.app.swap('');
			this.partial('haml/overview.haml');
    });

		this.get('#/events', function(context) {
		  this.get_page("/events.json", context, 'events');
			context.app.swap('');
			this.partial('haml/events.haml');
		});
		
		this.get('#/apps', function(context) {
			
			this.get_page("/apps.json", context, 'apps');
			
			var myint = setInterval(function(){
			  context.get_page("/apps.json", context, 'apps');
				context.partial('haml/apps/list.haml');
			}, 1000);
		  
      context.app.swap('');
			this.partial('haml/apps/list.haml');
		});
		
		this.get('#/apps/:name', function(context) {
		  this.get_page("/apps/"+ this.params["name"] + ".json", context, 'application');
      context.app.swap('');
			this.partial('haml/apps/show.haml');
		});
		
		this.get('#/bees', function(context) {
			context.get_page("/bees.json", context, 'bees');
			var myint = setInterval(function(){
			  context.get_page("/bees.json", context, 'bees');
				context.partial('haml/bees.haml');
			}, 1000); 
			
			context.app.swap('');
			context.partial('haml/bees.haml');
		});
		
		this.get('#/log', function(context) {
			context.app.swap('');
			this.partial('haml/log.haml');
		});
    
  });
  
  $(function() {
    app.run('#/');
  });
  
})(jQuery);