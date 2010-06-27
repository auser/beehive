$LAB

// jQuery
.script("/js/vendor/jquery-1.4.2.min.js").wait()

// sammy.js
.script("/js/vendor/sammy/sammy.js").wait()

// sammy plugins
.script("/js/vendor/sammy/plugins/sammy.template.js")
.script("/js/vendor/sammy/plugins/sammy.haml.js")
.script("/js/vendor/sammy/plugins/sammy.json.js").wait()

// sammy controllers
.script("/js/controllers/application_controller.js")
.script("/js/controllers/apps_controller.js")
.script("/js/controllers/bees_controller.js")
.script("/js/controllers/events_controller.js")
.script("/js/controllers/log_controller.js")
.script("/js/controllers/users_controller.js")
.script("/js/controllers/overview_controller.js").wait()

// sammy app
.script("/js/app.js").wait();