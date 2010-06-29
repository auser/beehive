// ----------------- //
// Bees "Controller" //
// ----------------- //

var bees_controller = function(app) {

  this.get('#/bees', function(context) {
  	this.get_page("/bees.json", "bees");
    this.auto_reload("/bees.json", "bees");
  });
  
};