require 'benchmark'

start_time = Time.now
puts "Hash table : Started at #{start_time}"
GC::Profiler.enable

N = 29

Benchmark.bm do |bench|
  N.times do |i| 
    bench.report("##{i}, objects: #{2**i}, ") do
      hash = {}
      (2**i).times do |x|
        hash[x] = x.to_s
      end
      pid, memory = `ps ax -o pid,rss | grep -e "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)
      puts "pid: #{pid}, memory:#{memory}" 
    end
  end 
end

GC.start
GC::Profiler.report

end_time = Time.now
puts "Ended at #{end_time}, cost:#{end_time - start_time}"

