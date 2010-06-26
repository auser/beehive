// ---------------- //
// Log "Controller" //
// ---------------- //

var log_controller = function(app) {
  
  this.get('#/log', function(context) {
  	context.app.swap('');
  	this.partial('/js/views/log/index.haml');
  });

}