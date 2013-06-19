namespace :server do
  desc 'Configures cabal'
  task :configure do
    sh 'cabal-dev configure --enable-tests --enable-benchmarks'
  end

  desc 'Build the server executable'
  task :build => [:configure] do
    sh 'cabal-dev build'
  end

  desc 'Runs server benchmarks'
  task :bench => [:build] do
    sh 'cabal-dev bench'
  end

  desc 'Runs server tests'
  task :test => [:build] do
    sh 'cabal-dev test'
  end
end
