require 'active_support/inflector'

libs = {
  amqp: { name: 'amqp', local_dependencies: [] },
  elastic_search: { name: 'elastic-search', local_dependencies: [:easy_api, :uri_template] },
  api: { name: 'easy-api', local_dependencies: [] },
  digitalocean: { name: 'digitalocean', local_dependencies: [] },
  github: { name: 'github', local_dependencies: ['easy-api', 'uri-template'] },
  hypermedia: { name: 'hypermedia', local_dependencies: [] },
  intercom: { name: 'intercom', local_dependencies: ['easy-api', 'uri-template'] },
  mandrill: { name: 'mandrill', local_dependencies: ['easy-api', 'uri-template'] },
  metrics: { name: 'metrics', local_dependencies: [] },
  newrelic: { name: 'newrelic', local_dependencies: [] },
  postgres_uuid: { name: 'postgres-simple-uuid', local_dependencies: [] },
  stripe: { name: 'stripe', local_dependencies: ['easy-api', 'uri-template'] },
  twilio: { name: 'twilio', local_dependencies: ['easy-api', 'uri-template'] },
  uri: { name: 'uri-template', local_dependencies: [] },
  redis_simple: { name: 'whodis', local_dependencies: [] }
}


task :default => (libs.keys.map {|k| "lib:#{k}:test" }) do

end

def cabal_tasks(lib_name, lib_info)
  namespace lib_name do
    dir = "lib/#{lib_info[:name]}"
    human_lib_name = ActiveSupport::Inflector.humanize lib_name
    tests_exist = false
    benchmarks_exist = false

    Dir.chdir(dir) do
      tests_exist = Dir.exists? "test"
      benchmarks_exist = Dir.exists? "bench"
    end

    desc "Configures the #{human_lib_name} library."
    task :configure do |task, args|
      Dir.chdir(dir) do
        if Dir.exists? 'dist'
          puts "#{human_lib_name} already configured. Nothing to be done."
        else
          sh 'cabal-dev configure --enable-tests --enable-benchmarks'
        end
      end
    end

    desc "Builds the #{human_lib_name} library."
    task :build => [:configure] do
      Dir.chdir(dir) do
        sh 'cabal-dev build'
      end
    end

    desc "Tests the #{human_lib_name} library."
    task :test => [:build] do
      Dir.chdir(dir) do
        sh 'cabal-dev test'
      end
    end

    desc :add_sources => do
      lib_info[:local_dependencies].each |d| do

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
  libs.each {|lib_name, lib_info| cabal_tasks lib_name, lib_info }
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
