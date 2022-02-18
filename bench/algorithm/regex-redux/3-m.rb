# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Rewrite for regex-redux by Aaron Tavistock
# array not dictionary by Isaac Gouy

class RegexRedux

    MATCHERS = [
      /agggtaaa|tttaccct/,
      /[cgt]gggtaaa|tttaccc[acg]/,
      /a[act]ggtaaa|tttacc[agt]t/,
      /ag[act]gtaaa|tttac[agt]ct/,
      /agg[act]taaa|ttta[agt]cct/,
      /aggg[acg]aaa|ttt[cgt]ccct/,
      /agggt[cgt]aa|tt[acg]accct/,
      /agggta[cgt]a|t[acg]taccct/,
      /agggtaa[cgt]|[acg]ttaccct/
    ]
  
  # ruby 1.8.7: to iterate in-order use array not dictionary
    FINAL_TRANSFORM = [
      [ /tHa[Nt]/, '<4>' ], 
      [ /aND|caN|Ha[DS]|WaS/, '<3>' ], 
      [ /a[NSt]|BY/, '<2>' ], 
      [ /<[^>]*>/, '|' ],
      [ /\|[^|][^|]*\|/, '-' ]
    ]
  
    def initialize(seq)
      @seq = seq
      @original_size = @seq.size
      @clean_size = remove_breaks!
      @match_results = match_results
      @final_size = final_transform!
    end
  
    def to_s
      "%s\n\n%d\n%d\n%d" % [
        @match_results.join("\n"),
        @original_size,
        @clean_size,
        @final_size
      ]
    end
  
    def pattern_count(regex)
      count = 0
      @seq.scan(regex) { count += 1 }
      "#{regex.source} #{count}"
    end 
  
    def forked_pattern_count(regex)
      reader, writer = IO.pipe
      Process.fork do
        reader.close
        writer.write(original_pattern_count(regex))
      end
  
      writer.close
      results = reader.read
      reader.close
    
      results
    end
  
  if (RUBY_PLATFORM != 'java') 
      alias_method :original_pattern_count, :pattern_count
      alias_method :pattern_count, :forked_pattern_count
    end
  
    def remove_breaks!
      @seq.gsub!(/>.*\n|\n/, '')
      @seq.size
    end
  
    def match_results
      threads = MATCHERS.map do |matcher|
        Thread.new do
          Thread.current[:result] = pattern_count(matcher)
        end
      end
      threads.each(&:join)
      threads.map { |t| t[:result] }
    end
  
    def final_transform!
      FINAL_TRANSFORM.each { |f,r| @seq.gsub!(f,r) }
      @seq.size
    end
  
  end
  
  fileName = ARGV[0] || "25000_in"
  regex_redux = RegexRedux.new(IO.readlines(fileName).join)
  puts regex_redux.to_s
