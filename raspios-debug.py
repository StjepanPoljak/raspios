import gdb

class RaspiosBreakpoint(gdb.Breakpoint):
    def __init__(self, spec):
        super(RaspiosBreakpoint, self).__init__(spec)
        print(f"[*] Breakpoint set at {spec}")

    def stop(self):
        # This function runs every time the breakpoint is hit
        print("[!] Breakpoint hit at:", hex(int(gdb.parse_and_eval("$pc"))))

        # You can evaluate or modify registers, memory, etc.
        x0 = int(gdb.parse_and_eval("$x0"))
        print(f"    x0 = 0x{x0:x}")

        # Example: dump 10 instructions at PC
        gdb.execute("x/10i $pc")

        # Return True to stop and give control to user,
        # or False to automatically continue execution.
        return True  # or False if you want it to auto-continue

class BreakFromOffset(gdb.Command):
    """Set breakpoint at symbol minus offset. Usage: bfo <symbol> <offset>"""

    def __init__(self):
        super(BreakFromOffset, self).__init__("bfo", gdb.COMMAND_BREAKPOINTS)

    def invoke(self, arg, from_tty):
        args = gdb.string_to_argv(arg)
        if len(args) != 2:
            print("Usage: bfo <symbol>")
            return

        sym_name = args[0]
        off_str = args[1]
        try:
            val = gdb.parse_and_eval("&" + sym_name)
            addr = int(val)
        except gdb.error:
            print(f"Symbol '{sym_name}' not found.")
            return
        #sym = gdb.lookup_symbol(sym_name)[0]
        #if not sym:
        #    print(f"Symbol '{sym_name}' not found.")
        #    return

        #addr = int(sym.value().address)
        offset = int(off_str, 16) #0x1000000 - 0x80000
        target = addr + offset
        print(f"[*] Breakpoint at 0x{target:x} ({sym_name} - 0x{offset:x})")
        RaspiosBreakpoint("*0x%x" % target)

BreakFromOffset()

low_offset = -0x1000000 + 0x80000
#low_offset = 0x0
high_offset = 0x0
#high_offset = 0x80000

gdb.execute("set architecture aarch64")
gdb.execute("target remote :1234")
gdb.execute("bfo test2 %x" % low_offset)
gdb.execute("continue")
#gdb.execute("b *0x0000000000080bd4")
#gdb.execute("continue")
limit = 0xffffff8000000000
#gdb.execute("bfo test %x" % high_offset)
#gdb.execute("continue")

while False:
    gdb.execute("thread 1")
    pc = int(gdb.parse_and_eval("$pc"))
    if pc == 0x200:
        print("Invalid address")
        break
    if pc >= limit:
        print(f"[+] Stopped: PC crossed 0x{limit:x} (now 0x{pc:x})")
        break
    gdb.execute("si", to_string=True)

