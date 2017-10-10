-- from penlight: 

local function call_ctor (c,obj,...)
    -- possible base call here
   local res = c._init(obj,...)
   return res
end

local function _class(base,c_arg,c)

   local mt = {}   -- a metatable for the class to support __call an

   c = c or {}
   c.__index = c
   setmetatable(c,mt)
   
   mt.__call = function(class_tbl,...)
      if rawget(c,'_init') then -- explicit constructor
	 local res = call_ctor(c,obj,...)
      end
   end
end


local List = { name = 'List' };
_class(nil,nil,List)

function List:_init (src)
   print "List__init called" 
   print (src);
end

List "a";
List {1,2,3};
