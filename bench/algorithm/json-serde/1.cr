require "json"
require "digest"

file_name = ARGV.size > 0 ? ARGV[0] : "sample"
n = ARGV.size > 1 ? ARGV[1].to_i : 3
file = File.new("#{file_name}.json")
content = file.gets_to_end
file.close
(0...n).each do |i|
  data = JSON.parse(content)
  io = IO::Memory.new
  builder = JSON::Builder.new(io)
  builder.indent = i + 1
  builder.start_document
  data.to_json(builder)
  builder.end_document
  # puts io.to_s
  hasher = Digest::MD5.new
  hasher.update(io.to_s)
  hash = hasher.final
  puts hash.map { |b| b.to_s(16) }.join
end
