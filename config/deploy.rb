set :stages, %w(production)
set :default_stage, "production"
require 'capistrano/ext/multistage'

set :application, "submarine"
set :user, "www-data"
set :group, "www-data"

set :scm, :git
set :repository, "git@github.com:iand675/#{application}.git"
set :deploy_to, "/var/www/#{application}"
set :deploy_via, :copy
set :build_script, "rake build"

