require "json"
require "http/server"
require "http/client"

def run_server(port : Int32)
  server = HTTP::Server.new do |context|
    body = context.request.body
    if !body.nil?
      req_content = context.request.body.as(IO).gets_to_end
      data = JSON.parse(req_content)
      n = data["value"]
    end
    context.response.print "#{n}"
  end

  address = server.bind_tcp "localhost", port, true
  # puts "Listening on http://#{address}"
  server.listen
end

def send(api : String, value : Int32, chan : Channel(Int32))
  payload = "{\"value\":#{value}}"
  while true
    begin
      response = HTTP::Client.post api, nil, payload
      content = response.body
      chan.send(content.to_i)
      return
    rescue
    end
  end
end

n = ARGV.size > 0 ? ARGV[0].to_i : 10
port = 30000 + Random.rand(10000)
spawn do
  run_server(port)
end
chan = Channel(Int32).new n
api = "http://localhost:#{port}/"
(1...(n+1)).each do |i|
  spawn do
     send(api, i, chan)
  end
end
sum = 0
(0...n).each do |i|
  sum += chan.receive
end
puts sum
