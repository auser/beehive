;(function($) {
  
  var PATH_REPLACER = "([^\/]+)",
      PATH_NAME_MATCHER = /:([\w\d]+)/g,
      QUERY_STRING_MATCHER = /\?([^#]*)$/,
      _decode = decodeURIComponent,
      loggers = [];
  
  Sammy = {};
  
  Sammy.VERSION = '0.4.1';
  
  // Add to the global logger pool. Takes a function that accepts an 
  // unknown number of arguments and should print them or send them somewhere
  // The first argument is always a timestamp.
  Sammy.addLogger = function(logger) {
    loggers.push(logger);
  };
  
  // Sends a log message to each logger listed in the global
  // loggers pool. Can take any number of arguments.
  // Also prefixes the arguments with a timestamp.
  Sammy.log = function()	{
    var args = $.makeArray(arguments);
    args.unshift("[" + Date() + "]");
    $.each(loggers, function(i, logger) {
      logger.apply(Sammy, args);
    });
	};
	
	if (typeof window.console != 'undefined') {
	  if ($.isFunction(console.log.apply)) {
      Sammy.addLogger(function() {
        window.console.log.apply(console, arguments);
      });
    } else {
      Sammy.addLogger(function() {
        window.console.log(arguments);
      });
    }
  } else if (typeof console != 'undefined') {
    Sammy.addLogger(function() {
      console.log.apply(console, arguments);
    });
  }
    
  // Sammy.Object is the base for all other Sammy classes. It provides some useful 
  // functionality, including cloning, iterating, etc.
  Sammy.Object = function(obj) { // constructor
    this.extend(obj);
  };
        
  $.extend(Sammy.Object.prototype, {    
    
    // Extend this object with the passed object
    extend: function(obj) {
      $.extend(this, obj);
    },
    
    // If passed an obj, clone the attributes and methods of that object
    // If called without arguments, clones the callee.
    clone: function(obj) {
      if (typeof obj == 'undefined') obj = this;
      return $.extend({}, obj);
    },
        
    // Returns a copy of the object with Functions removed.
    toHash: function() {
      var json = {}; 
      this.each(function(k,v) {
        if (!$.isFunction(v)) {
          json[k] = v
        }
      });
      return json;
    },
    
    // Renders a simple HTML version of this Objects attributes.
    // Does not render functions.
    // For example. Given this Sammy.Object:
    //    
    //    var s = new Sammy.Object({first_name: 'Sammy', last_name: 'Davis Jr.'});
    //    s.toHTML() //=> '<strong>first_name</strong> Sammy<br /><strong>last_name</strong> Davis Jr.<br />'
    //
    toHTML: function() {
      var display = "";
      this.each(function(k, v) {
        if (!$.isFunction(v)) {
          display += "<strong>" + k + "</strong> " + v + "<br />";
        }
      });
      return display;
    },
    
    // Generates a unique identifing string. Used for application namespaceing.
    uuid: function() {
      if (typeof this._uuid == 'undefined' || !this._uuid) {
        this._uuid = (new Date()).getTime() + '-' + parseInt(Math.random() * 1000);
      }
      return this._uuid;
    },
    
    // If passed an object and a callback, will iterate over the object
    // with (k, v) in the context of this object.
    // If passed just an argument - will itterate over 
    // the properties of this Sammy.Object
    each: function() {
      var context, object, callback, bound_callback;
      context = this;
      if (typeof arguments[0] != 'function') {
        object = arguments[0];
        callback = arguments[1];
      } else {
        object = this;
        callback = arguments[0];
      }
      bound_callback = function() {
        return callback.apply(context, arguments);
      }
      $.each(object, bound_callback);
    },
    
    // Returns an array of keys for this object. If <tt>attributes_only</tt> 
    // is true will not return keys that map to a <tt>function()</tt>
    keys: function(attributes_only) {
      var keys = [];
      for (var property in this) {
        if (!$.isFunction(this[property]) || !attributes_only) {
          keys.push(property);
        }
      }
      return keys;
    },
    
    // convenience method to join as many arguments as you want 
    // by the first argument - useful for making paths
    join: function() {
      var args = $.makeArray(arguments);
      var delimiter = args.shift();
      return args.join(delimiter);
    },
    
    // Shortcut to Sammy.log
    log: function() {
      Sammy.log.apply(Sammy, arguments);
    },
    
    // Returns a string representation of this object. 
    // if <tt>include_functions</tt> is true, it will also toString() the 
    // methods of this object. By default only prints the attributes.
    toString: function(include_functions) {
      var s = []
      this.each(function(k, v) {
		    if (!$.isFunction(v) || include_functions) {
          s.push('"' + k + '": ' + v.toString());
		    }
      });
      return "Sammy.Object: {" + s.join(',') + "}"; 
    }
  });
  
  // The HashLocationProxy is the default location proxy for all Sammy applications.
  // A location proxy is a prototype that conforms to a simple interface. The purpose
  // of a location proxy is to notify the Sammy.Application its bound to when the location
  // or 'external state' changes. The HashLocationProxy considers the state to be
  // changed when the 'hash' (window.location.hash / '#') changes. It does this in two
  // different ways depending on what browser you are using. The newest browsers 
  // (IE, Safari > 4, FF >= 3.6) support a 'onhashchange' DOM event, thats fired whenever
  // the location.hash changes. In this situation the HashLocationProxy just binds
  // to this event and delegates it to the application. In the case of older browsers
  // a poller is set up to track changes to the hash. Unlike Sammy 0.3 or earlier,
  // the HashLocationProxy allows the poller to be a global object, eliminating the
  // need for multiple pollers even when thier are multiple apps on the page.
  Sammy.HashLocationProxy = function(app, run_interval_every) {
    this.app = app;
    
    // check for native hash support
    if ('onhashchange' in window) {
      Sammy.log('native hash change exists, using');
      this.is_native = true;
    } else {
      Sammy.log('no native hash change, falling back to polling');
      this.is_native = false;
      this._startPolling(run_interval_every);
    }
  };
  
  Sammy.HashLocationProxy.prototype = {
    // bind the proxy events to the current app.
    bind: function() {
      var app = this.app;
      $(window).bind('hashchange.' + this.app.eventNamespace(), function() {
        app.trigger('location-changed');
      });
    },
    // unbind the proxy events from the current app
    unbind: function() {
      $(window).die('hashchange.' + this.app.eventNamespace());
    },
    // get the current location from the hash.
    getLocation: function() {
     // Bypass the `window.location.hash` attribute.  If a question mark
      // appears in the hash IE6 will strip it and all of the following
      // characters from `window.location.hash`.
      var matches = window.location.toString().match(/^[^#]*(#.+)$/);
      return matches ? matches[1] : '';
    },
    // set the current location to <tt>new_location</tt>
    setLocation: function(new_location) {
      return window.location = new_location;
    },
    
    _startPolling: function(every) {
      // set up interval
      var proxy = this;
      if (!Sammy.HashLocationProxy._interval) {
        if (!every) every = 10;
        var hashCheck = function() {
          current_location = proxy.getLocation();
          // Sammy.log('getLocation', current_location);
          if (!Sammy.HashLocationProxy._last_location || 
            current_location != Sammy.HashLocationProxy._last_location) {
            setTimeout(function() {
              $(window).trigger('hashchange');
            }, 1);
          }
          Sammy.HashLocationProxy._last_location = current_location;
        }
        hashCheck();
        Sammy.HashLocationProxy._interval = setInterval(hashCheck, every);
        $(window).bind('unload', function() {
          clearInterval(Sammy.HashLocationProxy._interval);
        });
      }
    }
  };
  
  // The DataLocationProxy is an optional location proxy prototype. As opposed to
  // the <tt>HashLocationProxy</tt> it gets its location from a jQuery.data attribute
  // tied to the application's element. You can set the name of the attribute by
  // passing a string as the second argument to the constructor. The default attribute
  // name is 'sammy-location'. To read more about location proxies, check out the 
  // documentation for <tt>Sammy.HashLocationProxy</tt>
  Sammy.DataLocationProxy = function(app, data_name) {
    this.app = app;
    this.data_name = data_name || 'sammy-location';
  };
  
  Sammy.DataLocationProxy.prototype = {
    bind: function() {
      var proxy = this;
      this.app.$element().bind('setData', function(e, key) {
        if (key == proxy.data_name) {
          proxy.app.trigger('location-changed');
        }
      });
    },
    
    unbind: function() {
      this.app.$element().die('setData');
    },
    
    getLocation: function() {
      return this.app.$element().data(this.data_name);
    },
    
    setLocation: function(new_location) {
      return this.app.$element().data(this.data_name, new_location);
    }
  };
  
  // Sammy.Application is the Base prototype for defining 'applications'.
  // An 'application' is a collection of 'routes' and bound events that is
  // attached to an element when <tt>run()</tt> is called.
  // The only argument an 'app_function' is evaluated within the context of the application.
  Sammy.Application = function(app_function) {
    var app = this;
    this.routes            = {};
    this.listeners         = new Sammy.Object({});
    this.befores           = [];
    this.namespace         = this.uuid();
    this.context_prototype = function() { Sammy.EventContext.apply(this, arguments) };
    this.context_prototype.prototype = new Sammy.EventContext();

    this.each(this.ROUTE_VERBS, function(i, verb) {
      this._defineRouteShortcut(verb);
    });
    if ($.isFunction(app_function)) {
      app_function.apply(this, [this]);
    }
    // set the location proxy if not defined to the default (HashLocationProxy)
    if (!this.location_proxy) {
      this.location_proxy = new Sammy.HashLocationProxy(app, this.run_interval_every);
    }
    if (this.debug) {
      this.bindToAllEvents(function(e, data) {
        app.log(app.toString(), e.cleaned_type, data || {});
      });
    }
  };
  
  Sammy.Application.prototype = $.extend({}, Sammy.Object.prototype, {
    
    // the four route verbs
    ROUTE_VERBS: ['get','post','put','delete'],
    
    // An array of the default events triggered by the 
    // application during its lifecycle
    APP_EVENTS: ['run','unload','lookup-route','run-route','route-found','event-context-before','event-context-after','changed','error-404','check-form-submission','redirect'],
    
    _last_route: null,
    _running: false,
    
    // On <tt>run()</tt> the application object is stored in a <tt>$.data</tt> entry
    // assocciated with the application's <tt>$element()</tt>
    data_store_name: 'sammy-app',
    
    // Defines what element the application is bound to. Provide a selector 
    // (parseable by <tt>jQuery()</tt>) and this will be used by <tt>$element()</tt>
    element_selector: 'body',
    
    // When set to true, logs all of the default events using <tt>log()</tt>
    debug: false,
    
    // When set to false, will throw a javascript error when a route is invoked
    // and can not be found.
    silence_404: true,
    
    // The time in milliseconds that the URL is queried for changes
    run_interval_every: 50, 
    
    // The location proxy for the current app. By default this is set to a new
    // <tt>Sammy.HashLocationProxy</tt> on initialization. However, you can set
    // the location_proxy inside you're app function to give youre app a custom
    // location mechanism
    location_proxy: null,
        
    // //=> Sammy.Application: body
    toString: function() {
      return 'Sammy.Application:' + this.element_selector;
    },
    
    // returns a jQuery object of the Applications bound element.
    $element: function() {
      return $(this.element_selector);
    },
    
    // <tt>use()</tt> is the entry point for including Sammy plugins.
    // The first argument to use should be a function() that is evaluated 
    // in the context of the current application, just like the <tt>app_function</tt>
    // argument to the <tt>Sammy.Application</tt> constructor.
    //
    // Any additional arguments are passed to the app function sequentially.
    //
    // For much more detail about plugins, check out: 
    // http://code.quirkey.com/sammy/doc/plugins.html
    // 
    // === Example
    //
    //      var MyPlugin = function(app, prepend) {
    //
    //        this.helpers({
    //          myhelper: function(text) {
    //            alert(prepend + " " + text);
    //          }
    //        });
    //  
    //      };
    //
    //      var app = $.sammy(function() {
    // 
    //        this.use(MyPlugin, 'This is my plugin');
    //  
    //        this.get('#/', function() {
    //          this.myhelper('and dont you forget it!'); 
    //          //=> Alerts: This is my plugin and dont you forget it!
    //        });
    //
    //      });
    //
    use: function() {
      // flatten the arguments
      var args = $.makeArray(arguments);
      var plugin = args.shift();
      try {
        args.unshift(this);
        plugin.apply(this, args);
      } catch(e) {
        if (typeof plugin == 'undefined') {
          throw("Error: called use() but plugin is not defined");
        } else if (!$.isFunction(plugin)) {
          throw("Error: called use() but '" + plugin.toString() + "' is not a function");
        } else {
          throw(e);
        }
      }
    },
    
    // <tt>route()</tt> is the main method for defining routes within an application.
    // For great detail on routes, check out: http://code.quirkey.com/sammy/doc/routes.html
    //
    // This method also has aliases for each of the different verbs (eg. <tt>get()</tt>, <tt>post()</tt>, etc.)
    //
    // === Arguments
    //
    // +verb+::     A String in the set of ROUTE_VERBS
    // +path+::     A Regexp or a String representing the path to match to invoke this verb.
    // +callback+:: A Function that is called/evaluated whent the route is run see: <tt>runRoute()</tt>
    //
    route: function(verb, path, callback) {
      // turn path into regex
      // create a simple object and add the route to it
      var app = this, 
          param_names = [],
          r;
      // if path is a string turn it into a regex
      if (path.constructor == String) {
        
        // Needs to be explicitly set because IE will maintain the index unless NULL is returned,
        // which means that with two consecutive routes that contain params, the second set of params will not be found and end up in splat instead of params
        // https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/RegExp/lastIndex        
        PATH_NAME_MATCHER.lastIndex = 0;
        
        // find the names
        while ((path_match = PATH_NAME_MATCHER.exec(path)) != null) {
          param_names.push(path_match[1]);
        }
        // replace with the path replacement
        path = new RegExp(path.replace(PATH_NAME_MATCHER, PATH_REPLACER) + "$");
      }
      r = {verb: verb, path: path, callback: callback, param_names: param_names};
      // add route to routes array
      if (typeof this.routes[verb] == 'undefined' || this.routes[verb].length == 0)  {
        // add to the front of an empty array
        this.routes[verb] = [r];
      } else {
        // place routes in order of definition
        this.routes[verb].push(r);
      }
      // return the route
      return r;
    },
    
    // A unique event namespace defined per application.
    // All events bound with <tt>bind()</tt> are automatically bound within this space.
    eventNamespace: function() {
      return [this.data_store_name, this.namespace].join('-');
    },
    
    // Works just like <tt>jQuery.fn.bind()</tt> with a couple noteable differences.
    //
    // * It binds all events to the application element
    // * All events are bound within the <tt>eventNamespace()</tt>
    // * Events are not actually bound until the application is started with <tt>run()</tt>
    // * callbacks are evaluated within the context of a Sammy.EventContext
    //
    // See http://code.quirkey.com/sammy/docs/events.html for more info.
    //
    bind: function(name, data, callback) {
      var app = this;
      // build the callback
      // if the arity is 2, callback is the second argument
      if (typeof callback == 'undefined') callback = data;
      var listener_callback =  function() {
        // pull off the context from the arguments to the callback
        var e, context, data; 
        e       = arguments[0];
        data    = arguments[1];        
        if (data && data['context']) {
          context = data['context']
          delete data['context'];
        } else {
          context = new app.context_prototype(app, 'bind', e.type, data);
        }
        e.cleaned_type = e.type.replace(app.eventNamespace(), '');
        callback.apply(context, [e, data]);
      };
      
      // it could be that the app element doesnt exist yet
      // so attach to the listeners array and then run()
      // will actually bind the event.
      if (!this.listeners[name]) this.listeners[name] = [];
      this.listeners[name].push(listener_callback);
      if (this.isRunning()) {
        // if the app is running
        // *actually* bind the event to the app element
        return this._listen(name, listener_callback);
      }
    },
    
    // Triggers custom events defined with <tt>bind()</tt>
    //
    // === Arguments
    // 
    // +name+::     The name of the event. Automatically prefixed with the <tt>eventNamespace()</tt>
    // +data+::     An optional Object that can be passed to the bound callback.
    // +context+::  An optional context/Object in which to execute the bound callback. 
    //              If no context is supplied a the context is a new <tt>Sammy.EventContext</tt>
    //
    trigger: function(name, data) {
      return this.$element().trigger([name, this.eventNamespace()].join('.'), [data]);
    },
    
    // Reruns the current route
    refresh: function() {
      this.last_location = null;
    },
    
    // Takes a single callback that is pushed on to a stack.
    // Before any route is run, the callbacks are evaluated in order within 
    // the current <tt>Sammy.EventContext</tt>
    //
    // If any of the callbacks explicitly return false, execution of any 
    // further callbacks and the route itself is halted.
    before: function(callback) {
      return this.befores.push(callback);
    },
    
    // A shortcut for binding a callback to be run after a route is executed.
    // After callbacks have no guarunteed order.
    after: function(callback) {
      return this.bind('event-context-after', callback);
    },
    
    // Returns a boolean of weather the current application is running.
    isRunning: function() {
      return this._running;
    },
    
    // Helpers extends the EventContext prototype specific to this app.
    // This allows you to define app specific helper functions that can be used
    // whenever you're inside of an event context (templates, routes, bind).
    // 
    // === Example
    //
    //    var app = $.sammy(function() {
    //      
    //      helpers({
    //        upcase: function(text) {
    //         return text.toString().toUpperCase();
    //        }
    //      });
    //      
    //      get('#/', function() { with(this) {
    //        // inside of this context I can use the helpers
    //        $('#main').html(upcase($('#main').text());
    //      }});
    //      
    //    });
    //
    //    
    // === Arguments
    // 
    // +extensions+:: An object collection of functions to extend the context.
    //  
    helpers: function(extensions) {
      $.extend(this.context_prototype.prototype, extensions);
    },
    
    // Helper extends the event context just like <tt>helpers()</tt> but does it
    // a single method at a time. This is especially useful for dynamically named 
    // helpers
    // 
    // === Example
    //     
    //     // Trivial example that adds 3 helper methods to the context dynamically
    //     var app = $.sammy(function(app) {
    //       
    //       $.each([1,2,3], function(i, num) {
    //         app.helper('helper' + num, function() {
    //           this.log("I'm helper number " + num);
    //         }); 
    //       });
    //       
    //       this.get('#/', function() {
    //         this.helper2(); //=> I'm helper number 2
    //       });
    //     });
    //     
    // === Arguments
    // 
    // +name+:: The name of the method
    // +method+:: The function to be added to the prototype at <tt>name</tt>
    //
    helper: function(name, method) {
      this.context_prototype.prototype[name] = method;
    },
    
    // Actually starts the application's lifecycle. <tt>run()</tt> should be invoked
    // within a document.ready block to ensure the DOM exists before binding events, etc.
    //
    // === Example
    // 
    //    var app = $.sammy(function() { ... }); // your application
    //    $(function() { // document.ready
    //        app.run();
    //     });
    //
    // === Arguments
    //
    // +start_url+::  "value", Optionally, a String can be passed which the App will redirect to 
    //                after the events/routes have been bound.
    run: function(start_url) {
      if (this.isRunning()) return false;
      var app = this;
      
      // actually bind all the listeners
      this.each(this.listeners.toHash(), function(name, callbacks) {
        this.each(callbacks, function(i, listener_callback) {
          this._listen(name, listener_callback);
        });
      });
      
      this.trigger('run', {start_url: start_url});
      this._running = true;
      // set data for app
      this.$element().data(this.data_store_name, this);
      // set last location
      this.last_location = null;
      if (this.getLocation() == '' && typeof start_url != 'undefined') {
        this.setLocation(start_url);
      } 
      // check url
      this._checkLocation();
      this.location_proxy.bind();
      this.bind('location-changed', function() {
        app._checkLocation();
      });
      
      // bind re-binding to after route
      this.bind('changed', function() {
        // bind form submission 
        app.$element()
          .find('form:not(.' + app.eventNamespace() + ')')
            .bind('submit', function() {
              return app._checkFormSubmission(this);
            })
            .addClass(app.eventNamespace());
      });
      // bind unload to body unload
      $('body').bind('onunload', function() {
        app.unload();
      });
      
      // trigger html changed
      this.trigger('changed');
    },
    
    // The opposite of <tt>run()</tt>, un-binds all event listeners and intervals
    // <tt>run()</tt> Automaticaly binds a <tt>onunload</tt> event to run this when
    // the document is closed.
    unload: function() {
      if (!this.isRunning()) return false;
      var app = this;
      this.trigger('unload');
      // clear interval
      this.location_proxy.unbind();
      // unbind form submits
      this.$element().find('form')
        .unbind('submit')
        .removeClass(app.eventNamespace());
      // clear data
      this.$element().removeData(this.data_store_name);
      // unbind all events
      this.each(this.listeners.toHash() , function(name, listeners) {
        this.each(listeners, function(i, listener_callback) {
          this._unlisten(name, listener_callback);
        });
      });
      this._running = false;
    },
    
    // Will bind a single callback function to every event that is already 
    // being listened to in the app. This includes all the <tt>APP_EVENTS</tt>
    // as well as any custom events defined with <tt>bind()</tt>.
    // 
    // Used internally for debug logging.
    bindToAllEvents: function(callback) {
      // bind to the APP_EVENTS first
      this.each(this.APP_EVENTS, function(i, e) {
        this.bind(e, callback);
      });
      // next, bind to listener names (only if they dont exist in APP_EVENTS)
      this.each(this.listeners.keys(true), function(i, name) {
        if (this.APP_EVENTS.indexOf(name) == -1) {
          this.bind(name, callback);
        }
      });
    },

    // Returns a copy of the given path with any query string after the hash
    // removed.
    routablePath: function(path) {
      return path.replace(QUERY_STRING_MATCHER, '');
    },
    
    // Given a verb and a String path, will return either a route object or false
    // if a matching route can be found within the current defined set. 
    lookupRoute: function(verb, path) {
      var routed = false;
      this.trigger('lookup-route', {verb: verb, path: path});
      if (typeof this.routes[verb] != 'undefined') {
        this.each(this.routes[verb], function(i, route) {
          if (this.routablePath(path).match(route.path)) {
            routed = route;
            return false;
          }
        });
      }
      return routed;
    },

    // First, invokes <tt>lookupRoute()</tt> and if a route is found, parses the 
    // possible URL params and then invokes the route's callback within a new
    // <tt>Sammy.EventContext</tt>. If the route can not be found, it calls 
    // <tt>notFound()</tt> and raise an error. If <tt>silence_404</tt> is <tt>true</tt>
    // this error will be caught be the internal methods that call <tt>runRoute</tt>.
    //
    // You probably will never have to call this directly.
    //
    // === Arguments
    // 
    // +verb+:: A String for the verb.
    // +path+:: A String path to lookup.
    // +params+:: An Object of Params pulled from the URI or passed directly.
    //
    // === Returns
    //
    // Either returns the value returned by the route callback or raises a 404 Not Found error.
    //
    runRoute: function(verb, path, params) {
      this.log('runRoute', [verb, path].join(' '));
      this.trigger('run-route', {verb: verb, path: path, params: params});
      if (typeof params == 'undefined') params = {};

      $.extend(params, this._parseQueryString(path));
      
      var route = this.lookupRoute(verb, path);
      if (route) {
        this.trigger('route-found', {route: route});
        // pull out the params from the path
        if ((path_params = route.path.exec(this.routablePath(path))) != null) {
          // first match is the full path
          path_params.shift();
          // for each of the matches
          this.each(path_params, function(i, param) {
            // if theres a matching param name
            if (route.param_names[i]) {
              // set the name to the match
              params[route.param_names[i]] = _decode(param);
            } else {
              // initialize 'splat'
              if (!params['splat']) params['splat'] = [];
              params['splat'].push(_decode(param));
            }
          });
        }
        
        // set event context
        var context  = new this.context_prototype(this, verb, path, params);
        this.last_route = route;
        // run all the before filters
        var before_value = true; 
        var befores = this.befores.slice(0);
        while (befores.length > 0) {
          if (befores.shift().apply(context) === false) return false;
        }
        context.trigger('event-context-before', {context: context});
        var returned = route.callback.apply(context, [context]);
        context.trigger('event-context-after', {context: context});
        return returned;
      } else {
        this.notFound(verb, path);
      }
    },
    
    // Delegates to the <tt>location_proxy</tt> to get the current location.
    // See <tt>Sammy.HashLocationProxy</tt> for more info on location proxies.
    getLocation: function() {
      return this.location_proxy.getLocation()
    },
    
    // Delegates to the <tt>location_proxy</tt> to set the current location.
    // See <tt>Sammy.HashLocationProxy</tt> for more info on location proxies.
    //
    // === Arguments
    // 
    // +new_location+:: A new location string (e.g. '#/')
    //
    setLocation: function(new_location) {
      return this.location_proxy.setLocation(new_location);
    },
    
    // Swaps the content of <tt>$element()</tt> with <tt>content</tt>
    // You can override this method to provide an alternate swap behavior
    // for <tt>EventContext.partial()</tt>.
    // 
    // === Example
    //
    //    var app = $.sammy(function() {
    //      
    //      // implements a 'fade out'/'fade in'
    //      this.swap = function(content) {
    //        this.$element().hide('slow').html(content).show('slow');
    //      }
    //      
    //      get('#/', function() {
    //        this.partial('index.html.erb') // will fade out and in
    //      });
    //      
    //    });
    //
    swap: function(content) {
      return this.$element().html(content);
    },
    
    // This thows a '404 Not Found' error.
    notFound: function(verb, path) {
      this.trigger('error-404', {verb: verb, path: path});
      throw('404 Not Found ' + verb + ' ' + path);
    },
        
    _defineRouteShortcut: function(verb) {
      var app = this;
      this[verb] = function(path, callback) {
        app.route.apply(app, [verb, path, callback]);
      }
    },
    
    _checkLocation: function() {
      try { // try, catch 404s
        // get current location
        var location, returned;
        location = this.getLocation();
        // compare to see if hash has changed
        if (location != this.last_location) {
          // lookup route for current hash
          returned = this.runRoute('get', location);
        }
        // reset last location
        this.last_location = location;
      } catch(e) {
        // reset last location
        this.last_location = location;
        // unless the error is a 404 and 404s are silenced
        if (e.toString().match(/^404/) && this.silence_404) {
          return returned;
        } else {
          throw(e);
        }
      }
      return returned;
    },
    
    _checkFormSubmission: function(form) {
      var $form, path, verb, params, returned;
      this.trigger('check-form-submission', {form: form});
      $form = $(form);
      path  = $form.attr('action');
      verb  = $.trim($form.attr('method').toString().toLowerCase());
      if (!verb || verb == '') { verb = 'get'; }
      params = $.extend({}, this._parseFormParams($form), {'$form': $form});
            
      try { // catch 404s
        returned = this.runRoute(verb, path, params);
      } catch(e) {
        if (e.toString().match(/^404/) && this.silence_404) {
          return true;
        } else {
          throw(e);
        }
      }
      return (typeof returned == 'undefined') ? false : returned;
    },
    
    _parseFormParams: function($form) {
      var params = {};
      $.each($form.serializeArray(), function(i, field) {
        if (params[field.name]) {
          if ($.isArray(params[field.name])) {
            params[field.name].push(field.value);
          } else {
            params[field.name] = [params[field.name], field.value];
          }
        } else {
          params[field.name] = field.value;
        }
      });
      return params;
    },
    
    _parseQueryString: function(path) {
      var query = {}, parts, pairs, pair, i;

      parts = path.match(QUERY_STRING_MATCHER);
      if (parts) {
        pairs = parts[1].split('&');
        for (i = 0; i < pairs.length; i += 1) {
          pair = pairs[i].split('=');
          query[pair[0]] = _decode(pair[1]);
        }
      }

      return query;
    },
    
    _listen: function(name, callback) {
      return this.$element().bind([name, this.eventNamespace()].join('.'), callback);
    },
    
    _unlisten: function(name, callback) {
      return this.$element().unbind([name, this.eventNamespace()].join('.'), callback);
    }

  });
  
  // <tt>Sammy.EventContext</tt> objects are created every time a route is run or a 
  // bound event is triggered. The callbacks for these events are evaluated within a <tt>Sammy.EventContext</tt>
  // This within these callbacks the special methods of <tt>EventContext</tt> are available.
  // 
  // === Example
  //
  //  $.sammy(function() { with(this) {
  //    // The context here is this Sammy.Application
  //    get('#/:name', function() { with(this) {
  //      // The context here is a new Sammy.EventContext
  //      if (params['name'] == 'sammy') {
  //        partial('name.html.erb', {name: 'Sammy'});
  //      } else {
  //        redirect('#/somewhere-else')
  //      }
  //    }});
  //  }});
  //
  // Initialize a new EventContext
  //
  // === Arguments
  //
  // +app+::    The <tt>Sammy.Application</tt> this event is called within.
  // +verb+::   The verb invoked to run this context/route.
  // +path+::   The string path invoked to run this context/route.
  // +params+:: An Object of optional params to pass to the context. Is converted
  //            to a <tt>Sammy.Object</tt>.
  Sammy.EventContext = function(app, verb, path, params) {
    this.app    = app;
    this.verb   = verb;
    this.path   = path;
    this.params = new Sammy.Object(params);
  }
   
  Sammy.EventContext.prototype = $.extend({}, Sammy.Object.prototype, {
    
    // A shortcut to the app's <tt>$element()</tt>
    $element: function() {
      return this.app.$element();
    },
            
    // Used for rendering remote templates or documents within the current application/DOM.
    // By default Sammy and <tt>partial()</tt> know nothing about how your templates
    // should be interpeted/rendered. This is easy to change, though. <tt>partial()</tt> looks
    // for a method in <tt>EventContext</tt> that matches the extension of the file you're
    // fetching (e.g. 'myfile.template' will look for a template() method, 'myfile.haml' => haml(), etc.)
    // If no matching render method is found it just takes the file contents as is.
    // 
    // === Caching
    //
    // If you use the <tt>Sammy.Cache</tt> plugin, remote requests will be automatically cached unless
    // you explicitly set <tt>cache_partials</tt> to <tt>false</tt>
    //
    // === Examples
    //
    // There are a couple different ways to use <tt>partial()</tt>:
    // 
    //      partial('doc.html');
    //      //=> Replaces $element() with the contents of doc.html
    //
    //      use(Sammy.Template); 
    //      //=> includes the template() method
    //      partial('doc.template', {name: 'Sammy'}); 
    //      //=> Replaces $element() with the contents of doc.template run through <tt>template()</tt>
    //
    //      partial('doc.html', function(data) {
    //        // data is the contents of the template.
    //        $('.other-selector').html(data); 
    //      });
    //
    partial: function(path, data, callback) {
      var file_data, 
          wrapped_callback,
          engine,
          cache_key = 'partial:' + path,
          context = this;

      if ((engine = path.match(/\.([^\.]+)$/))) { engine = engine[1]; }
      if (typeof callback == 'undefined') {
        if ($.isFunction(data)) {
          // callback is in the data position
          callback = data;
          data = {};
        } else {
          // we should use the default callback
          callback = function(response) {
            context.app.swap(response);
          }
        }
      }
      data = $.extend({}, data, this);
      wrapped_callback = function(response) {
        if (engine && $.isFunction(context[engine])) {
          response = context[engine].apply(context, [response, data]);
        } 
        callback.apply(context, [response]);
        context.trigger('changed');
      };
      if (this.app.cache_partials && this.cache(cache_key)) {
        // try to load the template from the cache
        wrapped_callback.apply(context, [this.cache(cache_key)])
      } else {
        // the template wasnt cached, we need to fetch it
        $.get(path, function(response) {
          if (context.app.cache_partials) context.cache(cache_key, response);
          wrapped_callback.apply(context, [response])
        });
      }
    },
    
    // Changes the location of the current window. If <tt>to</tt> begins with 
    // '#' it only changes the document's hash. If passed more than 1 argument
    // redirect will join them together with forward slashes.
    //
    // === Example
    //
    //      redirect('#/other/route');
    //      // equivilent to
    //      redirect('#', 'other', 'route');
    //
    redirect: function() {
      var to, args = $.makeArray(arguments), 
          current_location = this.app.getLocation();
      if (args.length > 1) {
        args.unshift('/');
        to = this.join.apply(this, args);
      } else {
        to = args[0];
      }
      this.trigger('redirect', {to: to});
      this.app.last_location = this.path;
      this.app.setLocation(to);
      if (current_location == to) {
        this.app.trigger('location-changed');
      }
    },
    
    // Triggers events on <tt>app</tt> within the current context.
    trigger: function(name, data) {
      if (typeof data == 'undefined') data = {}; 
      if (!data.context) data.context = this;
      return this.app.trigger(name, data);
    },
    
    // A shortcut to app's <tt>eventNamespace()</tt>
    eventNamespace: function() {
      return this.app.eventNamespace();
    },
    
    // Raises a possible <tt>notFound()</tt> error for the current path.
    notFound: function() {
      return this.app.notFound(this.verb, this.path);
    },
    
    // //=> Sammy.EventContext: get #/ {}
    toString: function() {
      return "Sammy.EventContext: " + [this.verb, this.path, this.params].join(' ');
    }
        
  });
  
  $.sammy = function(app_function) {
    return new Sammy.Application(app_function);
  };

})(jQuery);
