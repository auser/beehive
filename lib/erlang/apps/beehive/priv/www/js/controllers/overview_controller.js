// --------------------- //
// Overview "Controller" //
// --------------------- //

var overview_controller = function(app) {

  this.get('#/overview', function(context) {
    context.app.swap('');
  	this.partial('haml/overview.haml');
  });
  
};