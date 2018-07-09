require 'benchmark'

start_time = Time.now
puts "Started at #{start_time}"
GC::Profiler.enable

arr = []
N = 300
BATCH = 8_000_000
Benchmark.bm do |bench|
  N.times do |i|
    bench.report("#{i + 1}, objects: #{(i + 1) * BATCH}\t") do
      BATCH.times do |x|
	num = i * BATCH + x 
        arr << num
      end 
    end 
  end 
end

pid, memory = `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)

GC.start
GC::Profiler.report

end_time = Time.now
puts "Ended at #{end_time}, cost time: #{end_time - start_time}, memory: #{memory}"

