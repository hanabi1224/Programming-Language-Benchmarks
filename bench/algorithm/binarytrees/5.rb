# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Jesse Millikan
# Modified by Wesley Moxam
# Modified && Parallelised by Scott Leggett
# *reset*
# Thread.exclusive deprecated

require 'thread'

module MiniParallel
    class Worker
        def initialize(read, write)
            @read, @write = read, write
        end

        def close_pipes
            @read.close
            @write.close
        end

        def work(index)
            Marshal.dump(index, @write)
            Marshal.load(@read)
        end
    end

    def self.map(array, &block)
        work_in_processes(
            array,
            [array.size, core_count].min,
            &block
        )
    end

    def self.core_count
        @@core_count ||= IO.read("/proc/cpuinfo").scan(/^processor/).size
    end

    private

    def self.work_in_processes(array, count, &block)
        index = -1
        results, threads = [], []
        mx = Mutex.new        

        workers = create_workers(array, count, &block)

        workers.each do |worker|
            threads << Thread.new do
              loop do
                mx.synchronize do index += 1 end 
                break if index >= array.size
                results[index] = worker.work(index)
              end
              worker.close_pipes
            end
        end

        threads.each(&:join)
        Process.waitall

        results
    end

    def self.create_workers(array, count, &block)
        workers = []
        count.times do
            workers << create_worker(array, workers, &block)
        end
        workers
    end

    def self.create_worker(array, started_workers, &block)
        child_read, parent_write = IO.pipe
        parent_read, child_write = IO.pipe

        Process.fork do
            started_workers.each(&:close_pipes)

            parent_write.close
            parent_read.close

            process_incoming_jobs(child_read, child_write, array, &block)

            child_read.close
            child_write.close
        end

        child_read.close
        child_write.close

        Worker.new(parent_read, parent_write)
    end

    def self.process_incoming_jobs(read, write, array, &block)
        until read.eof?
            index = Marshal.load(read)
            Marshal.dump(block.call(array[index]), write)
        end
    end
end

def item_check(left, right)
    if left
        1 + item_check(*left) + item_check(*right)
    else
        1
    end
end

def bottom_up_tree(depth)
    if depth > 0
        depth -= 1
        [bottom_up_tree(depth), bottom_up_tree(depth)]
    else
        [nil, nil]
    end
end

max_depth = ARGV[0].to_i
min_depth = 4

max_depth = [min_depth + 2, max_depth].max

stretch_depth = max_depth + 1
stretch_tree = bottom_up_tree(stretch_depth)

puts "stretch tree of depth #{stretch_depth}\t check: #{item_check(*stretch_tree)}"
stretch_tree = nil

long_lived_tree = bottom_up_tree(max_depth)

if MiniParallel.core_count > 1
        MiniParallel.map(min_depth.step(max_depth, 2).to_a) do |depth|
            iterations = 2**(max_depth - depth + min_depth)

            check = 0

            (1..iterations).each do |i|
                check += item_check(*bottom_up_tree(depth))
            end

            "#{iterations}\t trees of depth #{depth}\t check: #{check}"
        end.each{|l| puts l}
else
    min_depth.step(max_depth, 2).each do |depth|
        iterations = 2**(max_depth - depth + min_depth)

        check = 0

        (1..iterations).each do |i|
            check += item_check(*bottom_up_tree(depth))
        end

        puts "#{iterations}\t trees of depth #{depth}\t check: #{check}"
    end
end

puts "long lived tree of depth #{max_depth}\t check: #{item_check(*long_lived_tree)}"