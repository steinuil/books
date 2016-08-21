# Monkeypatching
'my string'.succ #=> "my strinh"

class String
  def succ
    'watch this'
  end
end

'my string'.succ #=> "watch this"


# Refinements
# https://ruby-doc.org/core/doc/syntax/refinements_rdoc.html
module StringExtensions
  refine String do
    def reverse
      'it\'s joke'
    end
  end
end

'my string'.reverse #=> "gnirts ym"

module Stuff
  using StringExtensions
  'my *string*'.reverse #=> "it's joke"
end


# Class chain
class MyClass; end
MyClass.ancestors #=> [MyClass, Object, Kernel, BasicObject]

module M1; end
module M2; include M1; end
module M3; prepend M1; end
M1.ancestors #=> M1
M2.ancestors #=> [M2, M1]
M3.ancestors #=> [M1, M3]


# Private methods
class C1
  def initialize
    priv
  end

  def self.meth
    priv
  end

  def meth
    priv
  end

  private

  def priv
    'private'
  end
end
c = C1.new
c.meth                 #=> "private"
C1.meth rescue 'fail'  #=> "fail"
c.priv rescue 'fail'   #=> "fail"
c.send :priv           #=> "private"
