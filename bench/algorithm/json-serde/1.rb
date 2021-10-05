require 'json'
require 'digest'

def printHash(data)
    md5 = Digest::MD5.new
    s = JSON.generate(data)
    md5.update(s)
    puts md5.hexdigest
end

fileName = ARGV[0] || "sample"
n = ARGV.size > 1 ? Integer(ARGV[1]) : 10

jsonStr = File.read("#{fileName}.json")
data = JSON.parse(jsonStr)
printHash(data)
array = []
for i in 0...n
    array.append(JSON.parse(jsonStr))
end
printHash(array)
