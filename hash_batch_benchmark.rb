require 'benchmark'

start_time = Time.now
puts "Started at #{start_time}"
GC::Profiler.enable

# num	memory usage	Ruby	time	
# 350 	22,937,912 	v2.3.7	1062.447206
# 600 	22,456,972	v2.4.4	642.37688

hash = {}
N = 300
BATCH = 2_000_000
Benchmark.bm do |bench|
  N.times do |i|
    bench.report("#{i + 1}, objects: #{(i + 1) * BATCH}\t") do
      BATCH.times do |x|
	num = i * BATCH + x 
        hash[num] = num
      end 
    end 
  end 
end

pid, memory = `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)

GC.start
GC::Profiler.report

end_time = Time.now
puts "Ended at #{end_time}, cost time: #{end_time - start_time}, memory: #{memory}"

