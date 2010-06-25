(function($) {
    
  var app = $.sammy(function() {
    this.use(application_controller);
    
    this.use(apps_controller);
    this.use(bees_controller);
    this.use(events_controller);
    this.use(log_controller);
    this.use(overview_controller);
    

		this.get('#/', function(context) {
			this.redirect('#/overview');
		});  
    
  });
  
  $(function() {
    app.run('#/');
  });
  
})(jQuery);

