file_name = ARGV.size > 0 ? ARGV[0] : "25000_in"
file = File.new(file_name)
content = file.gets_to_end
ilen = content.size
content = content.gsub(/>.*\n|\n/, "")
clen = content.size
[/agggtaaa|tttaccct/,
 /[cgt]gggtaaa|tttaccc[acg]/,
 /a[act]ggtaaa|tttacc[agt]t/,
 /ag[act]gtaaa|tttac[agt]ct/,
 /agg[act]taaa|ttta[agt]cct/,
 /aggg[acg]aaa|ttt[cgt]ccct/,
 /agggt[cgt]aa|tt[acg]accct/,
 /agggta[cgt]a|t[acg]taccct/,
 /agggtaa[cgt]|[acg]ttaccct/].each do |p|
  count = 0
  pos = 0
  while true
    m = content.match(p, pos)
    if m.nil?
      break
    else
      count += 1
      pos = m.as(Regex::MatchData).end
    end
  end
  puts "#{p.source} #{count}"
end

[
  [/tHa[Nt]/, "<4>"],
  [/aND|caN|Ha[DS]|WaS/, "<3>"],
  [/a[NSt]|BY/, "<2>"],
  [/<[^>]*>/, "|"],
  [/\|[^|][^|]*\|/, "-"],
].each do |pair|
  content = content.gsub(pair[0], pair[1])
end

puts "\n#{ilen}\n#{clen}\n#{content.size}"
