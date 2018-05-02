require 'pry'

puts "Started at #{Time.now}"

arr = []
8.times do |i|
	arr[i] = Thread.new {
		eator = (1..25000000).map{ |x| [x, x.to_s] }.to_h
	}
end

p `ps ax -o pid,rss | grep -E "^[[:space:]]*#{$$}"`.strip.split.map(&:to_i)
		
arr.each { |t| t.join }

puts "Ended at #{Time.now}"

#binding.pry
