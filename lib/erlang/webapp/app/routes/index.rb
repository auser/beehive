class Beehive::Application < Sinatra::Base
  get '/' do
    haml :index
  end

  get '/about' do
    haml :about
  end
end