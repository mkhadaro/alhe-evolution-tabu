
import sys

f = open(sys.argv[2], "w")

out = sys.argv[3] + "<<-c("+','.join(open(sys.argv[1]).read().split("\n"))+")"
f.write(out[:-2]+')')
f.close()