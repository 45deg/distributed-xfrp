import sys
import re

times = dict()

for line in sys.stdin: 
  [s, i, ts] = line.split('|')
  m = re.search("{(\d+),(\d+),(\d+)}", ts)
  t = int(ts)
  if s == 'S' and int(i) < 100:
    times[i] = t
  if s == 'G' and int(i) < 100:
    times[i] = t - times[i]
  
for t in times.values():
  print(t / 1e+6)