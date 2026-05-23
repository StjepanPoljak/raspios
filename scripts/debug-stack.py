import gdb
import re

def log(line):
    print(line)
    with open("stack-debug.out", "a") as f:
        f.write(line + "\n")

class StackTraceBreakpoint(gdb.Breakpoint):

    def __init__(self, func_name):
        super().__init__(func_name)
        self.func_name = func_name
        self.func_count = 0
        self.active = False
        self.entry_sp = 0
        self.pushes = 0
        self.pops = 0

    def stop(self):
        if self.active:
            raise Exception("Recursion detected - stopping.")

        self.active = True
        self.entry_sp = int(gdb.parse_and_eval("$sp"))
        self.pushes = 0
        self.pops = 0

        log(f"[START] {self.func_name} SP={self.entry_sp:04x}")

        return True

    def trace_step(self):
        pc = int(gdb.parse_and_eval("$pc"))
        sp = int(gdb.parse_and_eval("$sp"))
        cs = int(gdb.parse_and_eval("$cs"))
        insn_full = gdb.execute("x/i $pc", to_string=True).strip()
        insn_op = re.search(r'^=>.*:\s*([^\s].*)$', insn_full).group(1).split(" ")
        insn, op = insn_op[0], insn_op[-1]
        if insn == op:
            op = ""

        match insn:
            case "push":
                if self.func_count == 0:
                    self.pushes += 1
            case "pop":
                if self.func_count == 0:
                    self.pops += 1
                if self.pops > self.pushes:
                    log(f"[FAIL] Extra pop detected at [{cs:04x}:{pc:04x}].")
                    return False
            case "call":
                self.func_count += 1
            case "ret":
                self.func_count -= 1
                if self.func_count < 0:
                    if sp != self.entry_sp:
                        log("[FAIL] Stack corrupted:")
                        log(f"    SP=0x{sp:04x} (exit) != SP=0x{self.entry_sp:04x} (entry)")
                        log(f"    pop count = {self.pops}")
                        log(f"    push count = {self.pushes}")
                    else:
                        log("[PASS] Stack corruption not detected.")
                    self.active = False
                    return False
            case _:
                return True

        log(f"[{cs:04x}:{pc:04x}] {insn} {op} (SP=0x{sp:04x})")

        return True

if __name__ == "__main__":

    gdb.execute("target remote :5555")
    gdb.execute("symbol-file ../build/arch/x86/bios-legacy/boot-stage1-5.elf")

    tracer = StackTraceBreakpoint("print_memory_map")
    gdb.execute("continue")
    while tracer.trace_step():
        gdb.execute("stepi", to_string=True)
