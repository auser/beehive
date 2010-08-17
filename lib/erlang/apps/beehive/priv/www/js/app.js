(function($) {

  var app = $.sammy(function() {
    this.use(Sammy.Storage);
    Sammy.store = new Sammy.Store({name: 'usercookie', type: 'cookie'});

    this.use(application_controller);

    this.use(apps_controller);
    this.use(bees_controller);
    this.use(events_controller);
    this.use(log_controller);
    this.use(login_controller);
    this.use(overview_controller);
    this.use(users_controller);

    Sammy.current_user = Sammy.store.get('user');

    if (Sammy.current_user == null) {
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

