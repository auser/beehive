// ----------------- //
// Apps "Controller" //
// ----------------- //

var apps_controller = function(app) {
    
  this.get('#/apps', function(context) {
    this.get_page("/apps.json", context, 'apps');

    var interval_id = setInterval(function(){
      context.get_page("/apps.json", context, 'apps');
      context.partial('/js/views/apps/index.haml');
    }, 5000);
    
    app.interval_ids.push(interval_id);
    
    context.app.swap('');
    this.partial('/js/views/apps/index.haml');
  });

  this.get('#/apps/new', function(context) {
    context.app.swap('');
    this.partial('/js/views/apps/new.haml');
  });
  
  this.post('#/apps/create', function(context) {
    var new_app = this.params;
    new_app.token = this.get_token("root@getbeehive.com", "test");

    this.post_page("/apps.json", new_app, context, "response");

    this.redirect('#/apps');
  });

  this.get('#/apps/delete/:name', function(context) {
    console.log("deleting " + this.params['name']);
    
    var my_data = this.json( {token: this.get_token("root@getbeehive.com", "test")} );
    console.log("sending data: " + my_data);
    
    $.ajax({
      type: "DELETE",
      async: false,
      url: "/apps/" + this.params['name'] + ".json",
      data: my_data,
      dataType: "json",
      error: function(XMLHttpRequest, textStatus, errorThrown){
        console.log("DELETE error:");
        console.log(XMLHttpRequest);
        console.log(textStatus);
        console.log(errorThrown);
      },
      success: function(data){
        console.log("DELETE success:");
        console.log(data);
      }
    });
    
    this.redirect('#/apps');
    
  });

  this.get('#/apps/:name', function(context) {
    this.get_page("/apps/"+ this.params['name'] + ".json", context, 'application');
    context.app.swap('');
    this.partial('/js/views/apps/show.haml');
  });
  
};

