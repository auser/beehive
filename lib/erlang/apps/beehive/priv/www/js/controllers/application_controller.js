// ------------------------ //
// Application "Controller" //
// ------------------------ //

var application_controller = function(app) {
  this.element_selector = '#main';    
  this.use(Sammy.Haml);
  this.use(Sammy.JSON);
  
  this.interval_ids = [];

  this.before(function() {
    var context = this;
    this.clear_intervals();
  });


  // ======= //
  // Helpers //
  // ======= //

  this.helpers({
    get_page: function(url, context, model) {
      console.log("initiating ajax get: " + url);
      $.ajax({
        type: "GET",
        async: false,
        url: url,
        dataType: "json",
        success: function(data){
          context[model] = data[model];
        }
      });
    },
  
    post_page: function(url, data, context, model) {
      console.log("initiating ajax post: " + url);
      $.ajax({
        type: "POST",
        async: false,
        url: url,
        data: this.json(data),
        dataType: "json",
        error: function(XMLHttpRequest, textStatus, errorThrown){
          console.log("POST error: ")
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
    
    clear_intervals: function() {
      $.each(app.interval_ids, function(index, interval_id) {
        clearInterval(interval_id);
      });
      app.interval_ids = [];
    },
    
    opts_to_string: function(opts) {
      var opts_string = "";
      $.each(opts, function(key, value) {
        opts_string = " " + opts_string + key + "=" + '"' + value + '"';
      });
      return opts_string;
    },
  
    link_to: function(link_text, url, opts) {
      if ( opts === undefined ) {
            opts = {};
      }
      var opts_string = this.opts_to_string(opts);
      console.log(opts_string);
      
      return '<a href="' + url + '"' + opts_string + '>' + link_text + '</a>';
    }
  
  });

};