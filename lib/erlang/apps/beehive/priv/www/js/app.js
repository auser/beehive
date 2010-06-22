(function($) {
    
  var app = $.sammy(function() {
    this.element_selector = '#main';    
    this.use(Sammy.Haml);
    this.use(Sammy.JSON);

    var interval_ids = [];

    this.before(function() {
      var context = this;
    
      jQuery.each(interval_ids, function(index, interval_id) {
        clearInterval(interval_id);
      });
      interval_ids = [];
    });
    
    
    // =================== //
    // Application Helpers //
    // =================== //
    
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
      },
      
      post_page: function(url, data, context, model) {
        console.log("incoming ajax post");
        $.ajax({
          type: "POST",
          async: false,
          url: url,
          data: this.json(data),
          dataType: "json",
          error: function(XMLHttpRequest, textStatus, errorThrown){
            console.log(XMLHttpRequest);
            console.log(textStatus);
            console.log(errorThrown);
          },
          success: function(data){
            context[model] = data;
          }
        });
      },
      
      link_to: function(link_text, url) {
        return '<a href="' + url + '">' + link_text + '</a>';
      }
      
    });


    // ======================= //
    // Application Controllers //
    // ======================= //

		this.get('#/', function(context) {
			this.redirect('#/overview');
		});
		
		
		// --------------------- //
    // Overview "Controller" //
    // --------------------- //
		
		this.get('#/overview', function(context) {
      context.app.swap('');
			this.partial('haml/overview.haml');
    });
    
    
    // ------------------- //
    // Events "Controller" //
    // ------------------- //

		this.get('#/events', function(context) {
		  this.get_page("/events.json", context, 'events');
			context.app.swap('');
			this.partial('haml/events.haml');
		});
		
		
    // ----------------- //
    // Apps "Controller" //
    // ----------------- //
		
		this.get('#/apps', function(context) {
			
			this.get_page("/apps.json", context, 'apps');
			
			var interval_id = setInterval(function(){
			  context.get_page("/apps.json", context, 'apps');
				context.partial('haml/apps/list.haml');
			}, 5000);
			
			interval_ids.push(interval_id);
		  
      context.app.swap('');
			this.partial('haml/apps/list.haml');
		});
		
		this.post('#/apps/create', function(context) {
		  
		  var auth_params = {email: "root@getbeehive.com", password: "test"};
		  var auth_url = "/auth.json";
		  
		  this.post_page(auth_url, auth_params, context, "auth_response");
      
      var token = context.auth_response.token;
      var new_app = this.params;
      new_app.token = token;
      
      this.post_page("/apps.json", new_app, context, "response");

			this.redirect('#/apps');
		});
		
		this.get('#/apps/new', function(context) {
      context.app.swap('');
			this.partial('haml/apps/new.haml');
		});
		
		this.get('#/apps/:name', function(context) {
		  this.get_page("/apps/"+ this.params['name'] + ".json", context, 'application');
      context.app.swap('');
			this.partial('haml/apps/show.haml');
		});
		
		
    // ----------------- //
    // Bees "Controller" //
    // ----------------- //
		
		this.get('#/bees', function(context) {
			context.get_page("/bees.json", context, 'bees');

			var interval_id = setInterval(function(){
			  context.get_page("/bees.json", context, 'bees');
				context.partial('haml/bees.haml');
			}, 5000); 
			
			interval_ids.push(interval_id);
			
			context.app.swap('');
			context.partial('haml/bees.haml');
		});
		
		
		// ---------------- //
    // Log "Controller" //
    // ---------------- //
		
		this.get('#/log', function(context) {
			context.app.swap('');
			this.partial('haml/log.haml');
		});
    
  });
  
  $(function() {
    app.run('#/');
  });
  
})(jQuery);