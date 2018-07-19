import sys
import re

from operator import itemgetter

pairs = []

for line in sys.stdin: 
  [a,b,c] = line.split('|')
  if a == 'S' or a == 'G':
    continue
  pairs.append((int(a), b, int(c)))

pairs = sorted(pairs, key=itemgetter(0))

tstart = pairs[0][0]
nums = dict()

for (t, i, n) in pairs:
  nums[i] = n
  print(str(t-tstart) + "\t" + str(sum(nums.values())))
  