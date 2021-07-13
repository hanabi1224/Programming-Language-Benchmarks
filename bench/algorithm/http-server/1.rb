# http_server.rb
require 'socket'
server = TCPServer.new 5678

while session = server.accept
  while line = session.gets # Read lines from socket
    puts line         # and print them
  end

  session.print "HTTP/1.1 200\r\n" # 1
  session.print "Content-Type: text/html\r\n" # 2
  session.print "\r\n" # 3
  session.print "Hello world! The time is #{Time.now}" #4

  session.close
end
