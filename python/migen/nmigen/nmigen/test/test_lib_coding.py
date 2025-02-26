from .tools import *
from ..hdl.ast import *
from ..hdl.dsl import *
from ..back.pysim import *
from ..lib.coding import *


class EncoderTestCase(FHDLTestCase):
    def test_basic(self):
        enc = Encoder(4)
        with Simulator(enc) as sim:
            def process():
                self.assertEqual((yield enc.n), 1)
                self.assertEqual((yield enc.o), 0)

                yield enc.i.eq(0b0001)
                yield Delay()
                self.assertEqual((yield enc.n), 0)
                self.assertEqual((yield enc.o), 0)

                yield enc.i.eq(0b0100)
                yield Delay()
                self.assertEqual((yield enc.n), 0)
                self.assertEqual((yield enc.o), 2)

                yield enc.i.eq(0b0110)
                yield Delay()
                self.assertEqual((yield enc.n), 1)
                self.assertEqual((yield enc.o), 0)

            sim.add_process(process)
            sim.run()


class PriorityEncoderTestCase(FHDLTestCase):
    def test_basic(self):
        enc = PriorityEncoder(4)
        with Simulator(enc) as sim:
            def process():
                self.assertEqual((yield enc.n), 1)
                self.assertEqual((yield enc.o), 0)

                yield enc.i.eq(0b0001)
                yield Delay()
                self.assertEqual((yield enc.n), 0)
                self.assertEqual((yield enc.o), 0)

                yield enc.i.eq(0b0100)
                yield Delay()
                self.assertEqual((yield enc.n), 0)
                self.assertEqual((yield enc.o), 2)

                yield enc.i.eq(0b0110)
                yield Delay()
                self.assertEqual((yield enc.n), 0)
                self.assertEqual((yield enc.o), 1)

            sim.add_process(process)
            sim.run()


class DecoderTestCase(FHDLTestCase):
    def test_basic(self):
        dec = Decoder(4)
        with Simulator(dec) as sim:
            def process():
                self.assertEqual((yield dec.o), 0b0001)

                yield dec.i.eq(1)
                yield Delay()
                self.assertEqual((yield dec.o), 0b0010)

                yield dec.i.eq(3)
                yield Delay()
                self.assertEqual((yield dec.o), 0b1000)

                yield dec.n.eq(1)
                yield Delay()
                self.assertEqual((yield dec.o), 0b0000)

            sim.add_process(process)
            sim.run()


class ReversibleSpec:
    def __init__(self, encoder_cls, decoder_cls, args):
        self.encoder_cls = encoder_cls
        self.decoder_cls = decoder_cls
        self.coder_args  = args

    def get_fragment(self, platform):
        m = Module()
        enc, dec = self.encoder_cls(*self.coder_args), self.decoder_cls(*self.coder_args)
        m.submodules += enc, dec
        m.d.comb += [
            dec.i.eq(enc.o),
            Assert(enc.i == dec.o)
        ]
        return m.lower(platform)


class HammingDistanceSpec:
    def __init__(self, distance, encoder_cls, args):
        self.distance    = distance
        self.encoder_cls = encoder_cls
        self.coder_args  = args

    def get_fragment(self, platform):
        m = Module()
        enc1, enc2 = self.encoder_cls(*self.coder_args), self.encoder_cls(*self.coder_args)
        m.submodules += enc1, enc2
        m.d.comb += [
            Assume(enc1.i + 1 == enc2.i),
            Assert(sum(enc1.o ^ enc2.o) == self.distance)
        ]
        return m.lower(platform)


class GrayCoderTestCase(FHDLTestCase):
    def test_reversible(self):
        spec = ReversibleSpec(encoder_cls=GrayEncoder, decoder_cls=GrayDecoder, args=(16,))
        self.assertFormal(spec, mode="prove")

    def test_distance(self):
        spec = HammingDistanceSpec(distance=1, encoder_cls=GrayEncoder, args=(16,))
        self.assertFormal(spec, mode="prove")
