class a:
    def __init__(self,i):
        self.i = i
    def f0(self):
        print("a.f0(%d)" %(self.i));
    def f1(self):
        print("a.f1");
    def c(self, i):
        f = [self.f0,
             self.f1];
        f[i]();

v = a(2)
v.c(0);
v.c(1);
