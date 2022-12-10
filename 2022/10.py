with open("input10.txt", "r") as input10:
    instructions = list(map(str.split, input10.read().splitlines()))


class CPU:
    def __init__(self):
        self.reg = 1
        self.cyc = 0
        self.signal = []
        self.pixels = ["." for _ in range(240)]

    def tick(self):
        self.cyc += 1
        # if self.cyc in [19]

        print(f"Tick to cycle {self.cyc}")
        # if self.cyc in [20, 60, 100, 140, 180, 220]:
        self.signal.append(self.reg * self.cyc)
        timer = self.cyc % 40
        if timer in [self.reg, self.reg + 1, self.reg - 1]:
            self.pixels[self.cyc] = "#"

    def incr(self, n):
        self.reg += n
        print(f"incr reg to {self.reg}")

    def read(self, instr):
        match instr:
            case ["addx", n]:
                n = int(n)
                self.tick()
                print(f"*** reg at {self.reg}")
                self.incr(n)
                self.tick()
            case ["noop"]:
                self.tick()


cpu = CPU()

for i in instructions:
    cpu.read(i)

print(sum(cpu.signal))
chunks = [cpu.pixels[x:x+40] for x in range(0, len(cpu.pixels), 40)]
for i in chunks:
    print("".join(i))