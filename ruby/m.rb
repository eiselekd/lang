module A
  def foo ;
    print("A.foo\n");
  end
end

class B
  include A;
end

print(A.instance_method(:foo).to_s + "\n");
b = B.new

class << b
  class << self
    class << self
      class << self
      end
    end
  end
end


b.method(:foo).call();

def B.m0;  end
print(B.method(:m0).to_s + "\n");
print(B.ancestors.to_s + "\n");

print(b.singleton_class.singleton_class.singleton_class.singleton_class.ancestors.to_s + "\n")
print(b.method(:f0).to_s + "\n");





     #                     +---------+             +--------+
     #                     |         |             |        |
     #     BasicObject     |   (BasicObject)       |  ((BasicObject))
     #         ^           |         ^             |        ^
     #         |           |         |             |        |
     #      Object         |      (Object)         |    ((Object))
     #         ^           |         ^             |        ^
     #         |           |         |             |        |
     #         +-------+   |         +--------+    |        |
     #                 |   |                  |    |        |
     #              Module |              (Module) |        |
     #                 ^   |                  ^    |        |
     #                 |   |                  |    |        |
     #               Class |               (Class) |        |
     #                 ^   |                  ^    |        |
     #                 +---+                  +----+        |
     #                                                      |
     # obj--->OtherClass---------->(OtherClass)---------->((OtherClass))
