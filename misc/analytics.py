import sys

# ./start.sh simple 1 >! retry.txt 2>/dev/null

def calc(file):
  times = dict()
  with open(file) as f:
    for line in f.readlines():
      [sig, num, t] = line.rstrip().split("|")
      num = int(num)
      t   = int(t)
      if sig == 'S':
        t = -t
      times[num] = times.get(num, 0) + t
  return sum(times.values()) / len(times)

print("retry: {:.2f} msec".format(calc("../erlang/retry.txt")/1000))
print("no-retry: {:.2f} msec".format(calc("../erlang/noretry.txt")/1000))