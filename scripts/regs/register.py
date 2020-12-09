#!/usr/bin/env python3

from collections import OrderedDict
import math

class Register():

    separator = "---------------------------------------------------------------"

    def __init__(self, name, value, size):

        self.name = name

        if value[0:2] == "0x":
            self.value = bin(int(value[2:], 16))[2:].zfill(size)
        elif value[0:2] == "0b":
            self.value = value[2:].zfill(size)
        else:
            self.value = bin(int(value, 10))[2:].zfill(size)

        self.size = size
        self.bits = OrderedDict()

    def add_field(self, name, start, end, f):
        self.bits[(start, end)] = (name, f)

    def add_bit(self, name, bit, f):
        self.bits[(bit, bit)] = (name, f)

    def interpret(self):

        print("\t{}".format(self.name))
        print("{}".format(Register.separator))

        for bitf, tpl in self.bits.items():
            bit_slice = self.value[self.size-bitf[1]-1:self.size-bitf[0]]

            print("{:<12} {:<8} {:<32} {}\n\t{}".format(
                tpl[0],
                "({})".format(bitf[0]) if bitf[0] == bitf[1] \
                                     else "({}:{})".format(bitf[0], bitf[1]),
                "0b{}".format(bit_slice), hex(int(bit_slice, 2)), tpl[1](bit_slice))
            )
            print("{}".format(Register.separator))

    @staticmethod
    def fbytes(size):
        size_dict = {
            0: "B",
            1: "kB",
            2: "MB",
            3: "GB",
            4: "TB",
            5: "PB",
            6: "EB"
        }
        scale = math.log2(size) // 10
        return "{}{} ({}B)".format(size / (1024**scale), size_dict[scale], size)

