// ----------------- //
// Bees "Controller" //
// ----------------- //

var bees_controller = function(app) {

  this.get('#/bees', function(context) {
  	context.get_page("/bees.json", context, 'bees');

  	var interval_id = setInterval(function(){
  	  context.get_page("/bees.json", context, 'bees');
  		context.partial('/js/views/bees/index.haml');
  	}, 5000); 
	
  	app.interval_ids.push(interval_id);
  });
  
};