# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
# Contributed by Sokolov Yura
# Modified by Chris Houhoulis (April 2013):
#   - made loops uglier to avoid the unnecessary overhead of blocks
#   - nicer naming for readability
# ported from ruby to crystal by hanabi1224

ARRAY_LENGTH = ARGV.size > 0 ? ARGV[0].to_i : 100

u = Array.new(ARRAY_LENGTH, 1)
v = [] of Float64

def eval_A(i,j)
  1.0/((i+j)*(i+j+1)/2+i+1)
end

def vector_times_array(vector)
  arr, i = [] of Float64, 0
  while i < ARRAY_LENGTH
    sum, j = Float64.new(0), 0
    while j < ARRAY_LENGTH
      sum += eval_A(i,j) * vector[j]
      j += 1
    end
    arr << sum
    i += 1
  end
  arr
end

def vector_times_array_transposed(vector)
  arr, i = [] of Float64, 0
  while i < ARRAY_LENGTH
    sum, j = Float64.new(0), 0
    while j < ARRAY_LENGTH
      sum += eval_A(j,i) * vector[j]
      j += 1
    end
    arr << sum
    i += 1
  end
  arr
end

def vector_times_array_times_array_transposed(vector)
  vector_times_array_transposed(vector_times_array(vector))
end

10.times do
  v = vector_times_array_times_array_transposed(u)
  u = vector_times_array_times_array_transposed(v)
end

vBv, vv, i = 0, 0, 0
while i < ARRAY_LENGTH
  vBv += u[i]*v[i]
  vv += v[i]*v[i]
  i += 1
end

print "%0.9f" % (Math.sqrt(vBv/vv)), "\n"
