#!/usr/bin/python3
for i in filter(lambda i: i%3==0, map(lambda i: i*i, range(1, 5000+1))):
  print(i)
