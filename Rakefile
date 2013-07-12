require 'active_support/inflector'

task :default do

end

def cabal_tasks(lib_name, dir)
  namespace lib_name do
    human_lib_name = ActiveSupport::Inflector.humanize lib_name
    tests_exist = false
    benchmarks_exist = false

    Dir.chdir(dir) do
      tests_exist = Dir.exists? "test"
      benchmarks_exist = Dir.exists? "bench"
    end

    desc "Configures the #{human_lib_name} library."
    task :configure do
      Dir.chdir(dir) do
        sh 'cabal-dev configure --enable-tests --enable-benchmarks'
      end
    end

    desc "Builds the #{human_lib_name} library."
    task :build => [:configure] do
      Dir.chdir(dir) do
        sh 'cabal-dev build'
      end
    end

    if tests_exist
      desc "Tests the #{human_lib_name} library."
      task :test => [:build] do
        Dir.chdir(dir) do
          sh 'cabal-dev test'
        end
      end
    end
  end
end

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

namespace :lib do
  libs = {
    amqp: 'amqp',
    elastic_search: 'elastic-search',
    api: 'easy-api',
    digitalocean: 'digitalocean',
    github: 'github',
    hypermedia: 'hypermedia',
    intercom: 'intercom',
    mandrill: 'mandrill',
    metrics: 'metrics',
    newrelic: 'newrelic',
    postgres_uuid: 'postgres-simple-uuid',
    stripe: 'stripe',
    twilio: 'twilio',
    uri: 'uri-template'
  }

  libs.each {|lib_name, dir_name| cabal_tasks lib_name, "lib/#{dir_name}" }
end

namespace :frontend do
  task :build do
  end

  desc 'Bundle all assets for deployment'
  task :bundle do
  end

  desc 'Run frontend tests'
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

namespace :mac do
  task :build do

  end
end

=begin
namespace :db do
  desc 'Migrate'
  task :migrate do
  end

  desc 'Roll back'
  task :rollback do
  end
end
=end
