use Rack::Static, 
  :urls => ["/js", "/css", "/bootstrap"],
  :root => "resources/public"

run lambda { |env|
  uri = env['REQUEST_URI']
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
