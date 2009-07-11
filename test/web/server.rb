require 'socket'
port = (ARGV[0] || 4040).to_i
s =  (ARGV[1] || 0).to_i
server = TCPServer.new('localhost', port)
while (session = server.accept)
  f = session.gets.split(" ")[1]
  p = "mp3" + f
  if File.exist?(p)
    e = File.extname(f)
    c = "application/octet-stream"
    File.open("/etc/mime.types").each { |line|
      a = line.split(" ")
      a.each { |ae| c = a[0] if e == ("."+ ae)}
    }
    session.print "HTTP/1.0 200/OK\r\nServer: " +
      "OcamlCSP\r\nContent-type: #{c}\r\n\r\n"
    io = File.open(p, "rb")
    while (not io.eof?)
      sleep(s/1000.0)
      buffer = io.read(256)
      begin
        session.write(buffer)
      rescue Exception=>e
        #if we can write we break out
        break
      end
    end
    io.close
  end
  session.close
end
