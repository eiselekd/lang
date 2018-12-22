# decorator with arguments returns a decorator that returns
# decorated function
def noaction(func):
    def gen(*args,**kwargs):
        func.expected=None
        return func("x",*args)
    return gen

# decorator with arguments returns a decorator that returns
# decorated function
def action(arg0):
    def decor(functowrap):
        def gen(*args,**kwargs):
            return functowrap(arg0, *args)
        return gen
    return decor

@action("a0") # there is a call to action() that will create the decorator
@noaction
def m(*args):
    print(str(args))

m("m0")
m("m0","m1")

