with open("input6.txt", "r") as input7:
    datastream_buffer = input7.read()


def check_marker(chars, size):
    return len(set(chars)) == size

def locate_marker(string, size):
    for n, c in enumerate(string):
        if check_marker(string[n:n+size], size):
            return n + size


print(locate_marker(datastream_buffer, size=4))
print(locate_marker(datastream_buffer, size=14))
