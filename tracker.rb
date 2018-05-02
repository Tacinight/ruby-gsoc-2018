pid = ARGV[0]

t = Time.now
puts "Started at #{t}"
loop do
	size = `ps ax -o pid,rss | grep -E "^[[:space:]]*#{pid}"`.strip.split.map(&:to_i)
	t2 = Time.now
	puts "#{pid}, #{size[1] / 1024}, #{t2 - t}"
	sleep(1)
end
