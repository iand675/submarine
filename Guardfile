ignore %r{^dist/}, %r{^cabal-dev/}

group :backend do
  guard :rake, :task => 'server:build' do
    watch(%r{^src/(.+)\.hs})
    watch(%r{.+\.cabal})
  end
end

