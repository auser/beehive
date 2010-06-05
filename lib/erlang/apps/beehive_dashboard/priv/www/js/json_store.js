(function($) {
  
  var app = $.sammy(function() {
    this.element_selector = '#main';    
    this.use(Sammy.Template);
    this.use(Sammy.Session);

    this.before(function() {
      // load the items
      var context = this;
      $.ajax({
        url: 'data/items.js', 
        dataType: 'json',
        async: false,
        success: function(items) {
          context.items = items;
        }
      });
    });

    this.get('#/', function(context) {
      context.app.swap('');
      $.each(context.items, function(i, item) {
        context.partial('templates/item.template', {id: i, item: item}, function(rendered) {
          context.$element().append(rendered);
        });
      });
    });
    
    this.get('#/item/:id', function(context) {
      this.item = this.items[this.params['id']];
      if (!this.item) { return this.notFound(); }
      this.partial('templates/item_detail.template');
    });

    this.post('#/cart', function(context) {
      var item_id = this.params['item_id'];
      // fetch the current cart
      var cart  = this.session('cart', function() {
        return {};
      });
      if (!cart[item_id]) {
        // this item is not yet in our cart
        // initialize its quantity with 0
        cart[item_id] = 0;
      }
      cart[item_id] += parseInt(this.params['quantity']);
      // store the cart
      this.session('cart', cart);
      this.trigger('update-cart');
    });
    
    this.bind('update-cart', function() {
      var sum = 0;
      $.each(this.session('cart') || {}, function(id, quantity) {
        sum += quantity;
      });
      $('.cart-info')
          .find('.cart-items').text(sum).end()
          .animate({paddingTop: '30px'})
          .animate({paddingTop: '10px'});
    });
    
    this.bind('run', function() {
      // initialize the cart display
      this.trigger('update-cart');
    });
    
  });
  
  $(function() {
    app.run('#/');
  });
  
})(jQuery);