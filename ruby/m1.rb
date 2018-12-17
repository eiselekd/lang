class OtherClass end
obj = OtherClass.new

#print(obj.class.ancestors.to_s + "\n");

print(obj.to_s+"\n");
print(obj.singleton_class.to_s+"\n");

print(obj.class.singleton_class.singleton_class.ancestors.to_s + "\n")
#print(obj.class.ancestors.to_s + "\n")
#print(obj.singleton_methods().to_s + "\n");
#obj.foo();


#[#<Class:#<Class:OtherClass>>, #<Class:#<Class:Object>>, #<Class:#<Class:BasicObject>>, #<Class:Class>, #<Class:Module>, #<Class:Object>, #<Class:BasicObject>, Class, Module, Object, Kernel, BasicObject]



     #
     #
     #     BasicObject
     #         ^
     #         |
     #      Object
     #         ^
     #         |
     #        OtherClass
     #         ^
     #         |
     # obj->#<Class:OtherClass>



#def obj.foo end
#obj.f


     #    BasicObject
     #         ^           +---------+                 +--------+
     #         |           |         |                 |        |
     #     Kernel          | #<Class:BasicObject>      | #<Class:#<Class:BasicObject>>
     #         ^           |         ^                 |        ^
     #         |           |         |                 |        |
     #      Object         |   #<Class:Object>         | #<Class:#<Class:Object>>
     #         ^           |         ^                 |        ^
     #         |           |         |                 |        |
     #         +-------+   |         +--------+        |        |
     #                 |   |                  |        |        |
     #              Module |           #<Class:Module> |        |
     #                 ^   |                  ^        |        |
     #                 |   |                  |        |        |
     #               Class |           #<Class:Class>  |        |
     #                 ^   |                  ^        |        |
     #                 +---+                  +--------+        |
     #                                                          |
     #
     # obj--->#<Class:OtherClass>--------------->#<Class:#<Class:OtherClass>>

#       obj.class.singleton_class.singleton_class
