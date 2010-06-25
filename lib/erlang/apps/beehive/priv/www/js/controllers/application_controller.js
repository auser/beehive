var application_controller = function(app) {
  this.element_selector = '#main';    
  this.use(Sammy.Haml);
  this.use(Sammy.JSON);
  
  console.log(app);
  this.interval_ids = [];

  this.before(function() {
    var context = this;

    jQuery.each(app.interval_ids, function(index, interval_id) {
      clearInterval(interval_id);
    });
    app.interval_ids = [];
  });


  // ======= //
  // Helpers //
  // ======= //

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
  
    get_token: function(email, password) {
      var auth = {};
      var auth_url = "/auth.json";
      var auth_params = {email: email, password: password};
		  this.post_page(auth_url, auth_params, auth, "response");
		  return auth.response.token;
    },
  
    link_to: function(link_text, url) {
      return '<a href="' + url + '">' + link_text + '</a>';
    }
  
  });

};