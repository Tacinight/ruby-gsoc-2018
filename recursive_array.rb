require 'pry'
require 'benchmark'

puts "Recirsove array : Started at #{Time.now}"

a = [1]

(1..150).each do |i| 
  Benchmark.bm do |bench|
    bench.report("#{i}\t") do
      200000.times do
        a = [a]
      end
    end 
  end 
end

p `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)

puts "Ended at #{Time.now}"

# binding.pry
