require "rack"

use Rack::Static, 
  :urls => ["/js", "/css", "/bootstrap", "/out", "/cmsnew.js", "/test.html" ],
  :root => "resources/public"

run lambda { |env|
  uri = env['PATH_INFO']

  base_dir = 'resources/public/'
  
  file = base_dir + (uri =~ /cmsnew/ ? 'cmsnew.html' : '')
  [
    200, 
    {
      'Content-Type'  => 'text/html', 
      'Cache-Control' => 'public, max-age=86400' 
   },
   File.open(file, File::RDONLY)
  ]
}
