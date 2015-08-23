#!/usr/bin/env ruby

require 'socket'

class DaytimeServer
  def initialize(host='127.0.0.1', port=8765)
    @host = host
    @port = port
    @server = TCPServer.open(host, port)
  end
  def start_message
    STDOUT.puts "The server is running on #{@host}:#{@port}"
    STDOUT.puts "Press CTL-C to terminate"
  end
  def serve_forever
    start_message
    while client = @server.accept
      line = "Time is: #{Time.now}"
      client.puts line
      client.close
    end
  end
end

host = '127.0.0.1'
port = 8765

if not ARGV.length.between?(0,2)
  puts 'usage: echod [HOST [PORT]]'
  Process.exit(1)
end

case ARGV.length
when 1
  host = ARGV[0]
when 2
  host = ARGV[0]
  port = ARGV[1].to_i
end

echod = DaytimeServer.new(host, port)
echod.serve_forever
