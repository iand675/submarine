# encoding: utf-8

require 'bundler'
require 'bundler/setup'
require 'berkshelf/thor'

class Tmux < Thor
  desc "start", "start tmux session for all submarine components"
  def start
    %x( tmux new-session "teamocil --layout submarine.yml" )
  end

  desc "reattach", "reattach to tmux session"
  def reattach
    %x( tmux attach-session -t submarine )
  end
end

=begin
class Cabal < Thor
  desc "init" "adds all libraries to the cabal-dev sandbox"
  def init
    cabal-dev add-source lib/*
  end
end
=end
