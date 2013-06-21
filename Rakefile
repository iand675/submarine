namespace :server do
  desc 'Configure cabal'
  task :configure do
    sh 'cabal-dev configure --enable-tests --enable-benchmarks'
  end

  desc 'Build the server executable'
  task :build => [:configure] do
    sh 'cabal-dev build'
  end

  desc 'Run server benchmarks'
  task :bench => [:build] do
    sh 'cabal-dev bench'
  end

  desc 'Run server tests'
  task :test => [:build] do
    sh 'cabal-dev test'
  end
end

namespace :web do
  desc 'Bundle all assets for deployment'
  task :bundle do
  end

  desc 'Run frontend tests'
  task :test do
  end
end

namespace :cli do
  desc 'Configure'
  task :configure do
  end

  desc 'Build'
  task :build do
  end

  desc 'Test'
  task :test do
  end
end

namespace :ios do
  desc 'Build'
  task :build do
  end

  desc 'Test'
  task :test do
  end
end

namespace :db do
  desc 'Migrate'
  task :migrate do
  end

  desc 'Roll back'
  task :rollback do
  end
end