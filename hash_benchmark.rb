require 'pry'
require 'benchmark'

puts "Hash table : Started at #{Time.now}"

N = 29

Benchmark.bm do |bench|
  N.times do |i| 
    bench.report("#{i} - #{2**i}\n") do
      hash = {}
      (2**i).times do |x|
        hash[x] = x.to_s
      end
      p `ps ax -o pid,rss | grep -e "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)
    end 
  end 
end


puts "Ended at #{Time.now}"

# binding.pry
