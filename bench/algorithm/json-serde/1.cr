require "json"
require "digest"

def print_hash(json)
  # io = IO::Memory.new
  # builder = JSON::Builder.new(io)
  # builder.start_document
  # json.to_json(builder)
  # builder.end_document
  hasher = Digest::MD5.new
  # hasher.update(io.to_s)
  # puts json.to_json
  hasher.update(json.to_json)
  hash = hasher.final
  puts hash.map { |b| b.to_s(16) }.join
end 

file_name = ARGV.size > 0 ? ARGV[0] : "sample"
n = ARGV.size > 1 ? ARGV[1].to_i : 10
file = File.new("#{file_name}.json")
content = file.gets_to_end
file.close
data = JSON.parse(content)
print_hash(data)
array = Array(JSON::Any).new
(0...n).each do |i|
  array.push(JSON.parse(content))
end
print_hash(array)
