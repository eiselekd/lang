from nmigen import *
from nmigen.cli import main

class Statevec:
    def __init__(self, width):
        self.v = Record([
            ("l", 1),
            ("m", 2),
        ])

    def get_fragment(self, platform):
        m = Module()
        m.d.comb += self.o.eq(self.a - self.b)
        return m.lower(platform)

class ALU:
    def __init__(self, width):
        self.sel = Signal(2)
        self.a   = Signal(width)
        self.b   = Signal(width)
        self.o   = Signal(width)
        self.co  = Signal()
        self.rec = Record([
            ("l", 1),
            ("m", 2),
        ])
        self.vec = Statevec(width)

    def get_fragment(self, platform):
        m = Module()
        m.d.comb += self.rec.eq(self.vec.v);
        with m.If(self.sel == 0b00):
            m.d.comb += self.o.eq(self.a | self.b)
            m.d.comb += self.rec.l.eq(self.a[0])
        with m.Elif(self.sel == 0b01):
            m.d.comb += self.o.eq(self.a & self.b)
        with m.Elif(self.sel == 0b10):
            m.d.comb += self.o.eq(self.a ^ self.b)
        with m.Else():
            m.d.comb += Cat(self.o, self.co).eq(self.a - self.b)
        return m.lower(platform)


if __name__ == "__main__":
    alu = ALU(width=16)
    main(alu, ports=[alu.rec.l, alu.sel, alu.a, alu.b, alu.o, alu.co])
