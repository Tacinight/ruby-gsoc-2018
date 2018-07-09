require 'benchmark'

start_time = Time.now
puts "Array Benchmark: Started at #{start_time}"

GC::Profiler.enable
N = 30

Benchmark.bm do |bench|
  N.times do |i|
    bench.report("#{i + 1}, objects: #{2**(i + 1)}, ") do
      a = [1]
      (2**i).times do |n|
        a = [a]
      end
      pid, memory = `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)
      puts "pid: #{pid}, memory: #{memory}"
    end 
    GC.start
  end 
end

GC::Profiler.report

end_time = Time.now
puts "Ended at #{end_time}, cost time: #{end_time - start_time}"

