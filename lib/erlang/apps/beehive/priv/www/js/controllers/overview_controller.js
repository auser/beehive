// --------------------- //
// Overview "Controller" //
// --------------------- //

var overview_controller = function(app) {

  this.get('#/overview', function(context) {
    context.app.swap('');
  	this.partial('/js/views/overview/index.haml');
  });
  
};