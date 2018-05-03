require 'pry'
require 'benchmark'

puts "Started at #{Time.now}"

hash = {}

# num	memory usage	Ruby	time	
# 350 	22,937,912 	v2.3.7	1062.447206
# 600 	22,456,972	v2.4.4	642.37688
(1..350).each do |i| 
  Benchmark.bm do |bench|
    bench.report("#{i}\t") do
      1000000.times do
        hash[rand] = rand
      end 
    end 
  end 
end

p `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)

puts "Ended at #{Time.now}"

binding.pry
