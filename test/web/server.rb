require 'socket'
port = (ARGV[0] || 4040).to_i
s =  (ARGV[1] || 3).to_i
server = TCPServer.new('localhost', port)
while (session = server.accept)
  sleep(s)
  g = session.gets
  f = g.split(" ")[1]
  p = "mp3" + f
  x = File.exist?(p)
  if x
    session.print "HTTP/1.1 200/OK\r\nServer: OcamlCSP\r\nContent-type: application/mp3\r\n\r\n"
    io = File.open(p, "rb")
    while (not io.eof?)
      sleep(s/1000.0)
      buffer = io.read(256)
      session.write(buffer)
    end
    io.close
  end
  session.close
end
