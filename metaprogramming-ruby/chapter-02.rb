# Monkeypatching
class String
  def to_alphanumeric
    gsub /[^|w|s]/, ''
  end
end

'my *string*'.to_alphanumeric


# Refinements
# https://ruby-doc.org/core/doc/syntax/refinements_rdoc.html
module StringExtensions
  refine String do
    def to_alphanumeric
      gsub /[^|w|s]/, ''
    end
  end
end

module Stuff
  using StringExtensions
  'my *string*'.to_alphanumeric
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
    puts 'private'
  end
end
c = C1.new
c.meth
C1.meth rescue puts 'Fails'
