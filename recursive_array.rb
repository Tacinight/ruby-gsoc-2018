require 'pry'
require 'benchmark'

puts "Recursive array : Started at #{Time.now}"

N = 30

Benchmark.bm do |bench|
  N.times do |i|
    bench.report("#{i} - #{2**i}\n") do
      a = [1]
      (2**i).times do |n|
        a = [a]
      end
      p `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)
    end 
  end 
end

puts "Ended at #{Time.now}"

# binding.pry
