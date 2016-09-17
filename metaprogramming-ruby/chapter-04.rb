# Blocks
def using obj
  begin
    yield
  ensure
    obj.dispose
  end
end


# Scope
v1 = 1
class A
  v2 = 2
  local_variables
  def method
    v3 = 3
    local_variables
  end
end

obj = A.new
obj.method #=> [:v3]
local_variables #=> [:v1, :obj]

A = Class.new do
  "v1 is #{v1}"

  define_method :method do
    "v1 is still #{v1}"
  end
end


# instance_eval
class B
  attr_reader :v
  def initialize
    @v = 1
  end
end

obj = B.new
obj.instance_eval do
  @v        #=> 1
  @v = 2
end
obj.v #=> 2


# Proc vs lambda
def test_proc
  p = Proc.new { return 10; 'a' }.call
  p * 2
end
test_proc #=> 10

def test_lambda
  l = lambda { return 10; 'a' }.call
  l * 2
end
test_lambda #=> 20

p = Proc.new { |a, b| [a, b] }
p.call 1, 2, 3 #=> [1, 2]
p.call 1       #=> [1, nil]

l = lambda { |a, b| [a, b] }
l.call 1, 2, 3 rescue 'wrong number of args'
l.call 1 rescue 'wrong number of args'


## pag 94
