import sys
from Sobol import pi2d

def main():
    # Make sure the kernel is compiled by invoking the function
    pi2d(1)
    print "OK"
    while True:
        line = sys.stdin.readline()
        if line == "EXIT\n":
            print "OK"
            exit(1)
        else:
            n = int(line)
            res = pi2d(n)
            print("RESULT %f" % res)

if __name__ == "__main__":
    main()
