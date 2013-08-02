module Colorize
  def colorize(text, color_code)
    "\033[#{color_code}m#{text}\033[0m"
  end

  {
    :black    => 30,
    :red      => 31,
    :green    => 32,
    :yellow   => 33,
    :blue     => 34,
    :magenta  => 35,
    :cyan     => 36,
    :white    => 37
  }.each do |key, color_code|
    define_method key do |text|
      colorize(text, color_code)
    end
  end
end

desc "Build all the things"
task :all do |t|
  include Colorize
  def cabal_install(src)
    puts(green "Building #{src}")
    cabal = 'cabal-dev --sandbox=cabal-dev'
    puts `#{cabal} install #{src} --force-reinstalls -v0`
    if $?.to_i != 0
      puts(red "Building #{src} failed")
      exit $?.to_i
    end
  end

  libs = [
    'amqp',
    'whodis',
    'postgres-simple-uuid',
    'easy-api',
    'uri-templater',
    'digitalocean',
    'stripe',
    'mandrill',
    'twilio',
    'webby',
    'metrics',
    'github',
    'elastic-search'
  ]

  libs.each do |lib|
    cabal_install "lib/#{lib}"
  end
end
