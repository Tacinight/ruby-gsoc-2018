code = <<END
def add_two(a: 2, b: 3)
  a + b
end
puts add_two(a: 1, b: 1)
END
# puts RubyVM::InstructionSequence.compile(code).disasm

class Hash
  def key?(val)
    puts "looking for val #{val}"
    false
  end
end

def add_two(a: 2, b: 3)
  a + b
end

puts add_two(a: 1, b: 1)

