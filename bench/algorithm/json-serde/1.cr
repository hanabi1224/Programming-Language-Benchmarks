require "json"
require "digest"

def print_hash(json)
  hasher = Digest::MD5.new
  hasher.update(json.to_json)
  hash = hasher.final
  puts hash.map { |b| b.to_s(16, precision: 2, upcase: false) }.join
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
