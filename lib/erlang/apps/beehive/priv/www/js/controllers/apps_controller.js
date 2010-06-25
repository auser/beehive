// ----------------- //
// Apps "Controller" //
// ----------------- //

var apps_controller = function(app) {
    
  this.get('#/apps', function(context) {

  	this.get_page("/apps.json", context, 'apps');

  	var interval_id = setInterval(function(){
  	  context.get_page("/apps.json", context, 'apps');
  		context.partial('haml/apps/list.haml');
  	}, 5000);

  	app.interval_ids.push(interval_id);

    context.app.swap('');
  	this.partial('haml/apps/list.haml');
  });

  this.post('#/apps/create', function(context) {

    var new_app = this.params;
    new_app.token = this.get_token("root@getbeehive.com", "test");

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
  
};

