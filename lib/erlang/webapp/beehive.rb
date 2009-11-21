class BeehiveApp < Sinatra::Base
  set :root, File.dirname(__FILE__)
  
  configure do
    set :static, true
    Compass.configuration.parse(File.join(self.root, 'config', 'compass.rb'))
    set :haml, { :format => :html5 }
    set :sass, Compass.sass_engine_options
  end
  
  def partial(page, options={})
    haml page, options.merge!(:layout => false)
  end
  
  get '/stylesheets/:name.css' do
    content_type 'text/css', :charset => 'utf-8'
    sass(:"stylesheets/#{params[:name]}", Compass.sass_engine_options)
  end
  
  get '/' do
    haml :index
  end
  
  get "/:name" do
    haml :"#{params[:name]}"
  end
  
end