ignore %r{^dist/}, %r{^cabal-dev/}

group :server do
  guard :rake, :task => 'server:build' do
    watch(%r{^src/(.+)\.hs})
    watch(%r{^src/(.+)\.cabal})
  end
end

group :lib do
  guard :rake, :task => 'lib:uri:build' do

  end

  guard :rake, :task => 'lib:newrelic:build' do

  end

  guard :rake, :task => 'lib:mandrill:build' do

  end

  guard :rake, :task => 'lib:intercom:build' do

  end

  guard :rake, :task => 'lib:digitalocean:build' do

  end

  guard :rake, :task => 'lib:github:build' do

  end

  guard :rake, :task => 'lib:stripe:build' do

  end

  guard :rake, :task => 'lib:elasticsearch:build' do

  end

  guard :rake, :task => 'lib:api:build' do

  end
end

group :frontend do
  guard :rake, :task => 'frontend:build' do

  end
end

group :ios do
  guard :rake, :task => 'ios:build' do

  end
end

group :mac do
  guard :rake, :task => 'mac:build' do
  end
end
