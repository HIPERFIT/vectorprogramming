import sys
from Binomial import binom

def main():
    # Make sure the kernel is compiled by invoking the function
    binom(True,60.0,65.0,1,1)
    print "OK"
    while True:
        line = sys.stdin.readline()
        if line == "EXIT\n":
            print "OK"
            exit(1)
        else:
            numSteps = int(line)
            res = binom(True,60.0,65.0,1,numSteps)
            print("RESULT %f" % res)

if __name__ == "__main__":
    main()
