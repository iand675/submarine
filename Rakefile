require 'active_support/inflector'

task :default do

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
    digitalocean: 'digital-ocean',
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

  libs.each do |k, v|
    namespace k do
      humanized = ActiveSupport::Inflector.humanize k

      desc "Configures the #{humanized} library."
      task :configure do
        Dir.chdir("lib/#{v}") do
          sh 'cabal-dev configure --enable-tests --enable-benchmarks'
        end
      end

      desc "Builds the #{humanized} library."
      task :build => [:configure] do
        Dir.chdir("lib/#{v}") do
          sh 'cabal-dev build'
        end
      end

      desc "Tests the #{humanized} library."
      task :test => [:build] do
        Dir.chdir("lib/#{v}") do
          sh 'cabal-dev test'
        end
      end

      #task :bench do
      #end
    end
  end
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

namespace :db do
  desc 'Migrate'
  task :migrate do
  end

  desc 'Roll back'
  task :rollback do
  end
end
