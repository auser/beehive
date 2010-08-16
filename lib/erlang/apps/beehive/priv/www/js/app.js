(function($) {
    
  var app = $.sammy(function() {
    this.user = this.user ? this.user : {};
    
    this.use(application_controller);
    
    this.use(apps_controller);
    this.use(bees_controller);
    this.use(events_controller);
    this.use(log_controller);
    this.use(login_controller);
    this.use(overview_controller);
    this.use(users_controller);
    
    
    if (this.user.email == undefined) {
      this.get('#/', function(context) {this.redirect('#/login');})
      console.log("login");
    } else {
		  this.get('#/', function(context) {this.redirect('#/overview');});
		  console.log("overview");
		}  
  });
  
  $(function() {
    app.run('#/');
  });
  
})(jQuery);

