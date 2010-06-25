// ---------------- //
// Log "Controller" //
// ---------------- //

var log_controller = function(app) {
  
  this.get('#/log', function(context) {
  	context.app.swap('');
  	this.partial('haml/log.haml');
  });

}