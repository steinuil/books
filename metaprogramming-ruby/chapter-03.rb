# Calling methods dynamically
class MyClass
  def my_method arg
    arg * 2
  end

  private

  def priv_method arg
    arg * 3
  end
end

obj = MyClass.new
obj.my_method 3                #=> 6
obj.send :my_method, 3         #=> 6
obj.priv_method rescue 'fail'  #=> "fail"
obj.send :priv_method, 3       #=> 9


# Defining methods dynamically
class MyClass
  define_method :my_method do |arg|
    arg * 3
  end
end

MyClass.instance_methods false #=> [:my_method]


# method_missing
class Missing
  def method_missing message, *args
    args ||= ['-']
    %(Sent "#{message}" with these args: #{args.join ', '})
  end
end

mis = Missing.new
mis.method                  #=> "Sent \"method\" with these args: -"
mis.send :method, :tfw      #=> "Sent \"method\" with these args: tfw"
Missing.respond_to? :method #=> false
Missing.instance_methods false #=> [:method_missing]

mis.display ##<Missing:0x007f96b38ce028>=> nil
Object.instance_methods.include? :display #=> true
Object.instance_methods.count #=> 56
BasicObject.instance_methods.count #=> 8

class Missing < BasicObject
  def method_missing message, *args
    args ||= ['-']
    %(Sent "#{message}" with these args: #{args.join ', '})
  end
end

mis.display #=> "Sent \"display\" with these args: -"

class Sketchbook
  @@valid = [:sora, :hazuki, :natsumi]
  def method_missing message
    super unless @@valid.include? message
    "#{message.capitalize} is a main character from Sketchbook."
  end

  def respond_to_missing? message, include_private = false
    @@valid.include? message
  end
end

sketch = Sketchbook.new
sketch.respond_to? :sora   #=> true
sketch.respond_to? :nagisa #=> false
Sketchbook.instance_methods false #=> [:method_missing]


# const_missing
GF rescue 'no such const' #=> "no such const"

class Module
  alias :original_const_missing :const_missing

  def const_missing name
    if name == :GF
      'still no gf'
    else
      original_const_missing name
    end
  end
end

GF #=> "still no gf"


# 
