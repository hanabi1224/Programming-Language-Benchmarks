require 'json'
require 'digest'

fileName = ARGV[0] || "sample"
n = ARGV.size > 1 ? Integer(ARGV[1]) : 3

jsonStr = File.read("#{fileName}.json")

indent = " "
for i in 0...n
    data = JSON.parse(jsonStr)
    prettified = JSON.pretty_generate(data, {indent:indent})
    indent += " "
    md5 = Digest::MD5.new
    md5.update(prettified)
    puts md5.hexdigest
end
