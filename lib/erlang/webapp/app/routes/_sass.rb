class Beehive::Application < Sinatra::Base
  get '/css/reset.css' do
    headers 'Content-Type' => 'text/css; charset=utf-8'
    sass :'sass/reset'
  end

  get '/css/screen.css' do
    headers 'Content-Type' => 'text/css; charset=utf-8'
    sass :'sass/screen'
  end
end