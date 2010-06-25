(function($) {
    
  var app = $.sammy(function() {
    this.use(application_controller);
    this.use(apps_controller);
    this.use(bees_controller);


    // =========== //
    // Controllers //
    // =========== //

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