// ----------------- //
// Apps "Controller" //
// ----------------- //

var apps_controller = function(app) {
    
  this.get('#/apps', function(context) {
    this.get_page("/apps.json", 'apps');
    this.auto_reload("/apps.json", "apps");
  });

  this.get('#/apps/new', function(context) {
    $.ajax({
      type: "GET",
      async: false,
      url: "/system/config/app_templates.json",
      dataType: "json",
      success: function(data){
        app_types = data.types;
      }
    });
    console.log(app_types);
  });
  
  this.post('#/apps/create', function(context) {
    var new_app = this.params;
    new_app.token = this.get_token("root@getbeehive.com", "test");
    this.post_page("/apps.json", new_app, context, "response");
    this.redirect('#/apps');
  });

  this.get('#/apps/delete/:name', function(context) {
    console.log("deleting " + this.params['name']);
    
    var token = this.json( {token: this.get_token("root@getbeehive.com", "test")} );
    var url = "/apps/" + this.params['name'] + ".json";
    
    this.delete_page(url, token, context, "response");
    this.redirect('#/apps');
  });

  this.get('#/apps/:name', function(context) {
    this.get_page("/apps/"+ this.params['name'] + ".json", 'application');
  });
  
};