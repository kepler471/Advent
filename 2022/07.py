from pathlib import Path

with open("input7.txt", "r") as input7:
    term_out = input7.read().splitlines()


class File:
    def __init__(self, name: str, size: int):
        self.name = name
        self.size = size

    def __str__(self):
        return f"{self.name}: {self.size}"

    def __repr__(self):
        return f"File({self.name}: {self.size})"


class FileSystem:
    def __init__(self, root: str):
        self.root = Path(root)
        self.dirs = {self.root: []}
        self.cwd = self.root

    def setwd(self, directory: Path):
        self.cwd = directory

    def cd(self, arg: str):
        match arg:
            case "..":
                self.setwd(self.cwd.parent)
            case "/":
                self.setwd(Path("/"))
            case d:
                directory = self.cwd / Path(d)
                if directory in self.dirs.keys():
                    self.setwd(directory)

    def read_output(self, line):
        line = line.split()
        match line:
            case ["$", "cd", x]:
                self.cd(x)
            case ["$", _]:
                pass
            case ["dir", dirname]:
                self.dirs[self.cwd / Path(dirname)] = []
            case [size, name]:
                self.dirs[self.cwd].append(File(name, int(size)))


fs = FileSystem("/")

for i in term_out[1:]:
    fs.read_output(i)

files = []
for path in fs.dirs.keys():
    fs.setwd(path)
    files += [(fs.cwd, file.name, file.size) for file in fs.dirs[path]]

dirsizes = {x: 0 for x in fs.dirs.keys()}

for file in files:
    if file[0] in dirsizes:
        dirsizes[file[0]] += file[2]

    parents = file[0].parents
    for parent in parents:
        if parent in dirsizes:
            dirsizes[parent] += file[2]

print(sum([d if d <= 100000 else 0 for d in dirsizes.values()]))

total = 70_000_000
used = dirsizes[fs.root]
free = total - used
need = 30_000_000
required_deletion = need - free
suitable_deletions = [(path, dirsizes[path]) for path in sorted(dirsizes, key=lambda d: dirsizes[d]) if
                      dirsizes[path] > required_deletion]

print(suitable_deletions[0])
