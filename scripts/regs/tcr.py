#!/usr/bin/env python3

from register import Register
import sys

class TCR(Register):

    def __init__(self, value):
        Register.__init__(self, "Translation Control Register (TCR)", value, 64)

        self.add_field("T0SZ", 0, 5, self.get_tXsz)
        self.add_bit("EPD0", 7, self.get_epdX)
        self.add_field("IRGN0", 8, 9, lambda x: self.get_irgnX(x, "Inner"))
        self.add_field("ORGN0", 10, 11, lambda x: self.get_irgnX(x, "Outer"))
        self.add_field("SH0", 12, 13, self.get_shX)
        self.add_field("TG0", 14, 15, lambda x: self.get_tgX(x, 0))

        self.add_field("T1SZ", 16, 21, self.get_tXsz)
        self.add_bit("A1", 22, self.get_a1)
        self.add_bit("EPD1", 23, self.get_epdX)
        self.add_field("IRGN1", 24, 25, lambda x: self.get_irgnX(x, "Inner"))
        self.add_field("ORGN1", 26, 27, lambda x: self.get_irgnX(x, "Outer"))
        self.add_field("SH1", 28, 29, self.get_shX)
        self.add_field("TG1", 30, 31, lambda x: self.get_tgX(x, 1))
        self.add_field("IPS", 32, 34, self.get_ips)

        self.add_bit("AS", 36, self.get_as)
        self.add_bit("TBI0", 37, self.get_tbiX)
        self.add_bit("TBI1", 38, self.get_tbiX)

    def get_tXsz(self, field):
        return "region size: {}".format(Register.fbytes(2**(64 - int(field, 2))))

    def get_epdX(self, bit):
        return "perform table walk" if bit == "0" else "generate translation fault"

    def get_irgnX(self, field, sh):
        return {
            "00": "Normal memory, {} Non-cacheable",
            "01": "Normal memory, {} Write-Back Read-Allocate Write-Allocate Cacheable",
            "10": "Normal memory, {} Write-Through Read-Allocate No Write-Allocate Cacheable",
            "11": "Normal memory, {} Write-Back Read-Allocate No Write-Allocate Cacheable"
        }[field].format(sh)

    def get_shX(self, field):
        return {
            "00": "Non-shareable",
            "10": "Outer-shareable",
            "11": "Inner-shareable",
            "01": "INVALID"
        }[field]

    def get_tgX(self, field, table):
        return {
            "00": "4kB",
            "01": "64kB",
            "10": "16kB",
            "11": "INVALID"
        }[field] if table == 0 else {
            "00": "INVALID",
            "01": "16kB",
            "10": "4kB",
            "11": "64kB"
        }[field]

    def get_tbiX(self, bit):
        return {
            "0": "Top Byte used in address calculation.",
            "1": "Top Byte ignored in address calculation."
        }[bit]

    def get_a1(self, bit):
        return {
            "0": "TTBR0_EL1 defines ASID",
            "1": "TTBR1_EL1 defines ASID"
        }[bit]

    def get_ips(self, field):
        return {
            "000": "32 bits, 4GB",
            "001": "36 bits, 64GB",
            "010": "40 bits, 1TB",
            "011": "42 bits, 4TB",
            "100": "44 bits, 16TB",
            "101": "48 bits, 256TB",
            "110": "52 bits, 4PB"
        }[field]

    def get_as(self, bit):
        return {
            "0": "8-bit ASID size",
            "1": "16-bit ASID size"
        }[bit]

if __name__ == "__main__":

    TCR(sys.argv[1]).interpret()
