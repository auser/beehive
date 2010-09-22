// --------------------- //
// Login "Controller" //
// --------------------- //

var login_controller = function(app) {

  this.get('#/login', function(context) {
             this.error = false;
           });

  this.post('#/login', function(context) {
              var auth = this.params;
              this.post_page("/auth.json", auth, context, "response");
              if(context['error']) {
                this.error = true;
              } else {
                Sammy.current_user = context['response'];
                Sammy.store.set('user', context['response']);
                this.redirect('#/overview');
              }
            });

};
