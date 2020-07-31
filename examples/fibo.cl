var a, b, i, t;
mem n = 10;

a = 0
b = 1
i = 0
while i < n {
  print b
  t = a + b
  a = b
  b = t
  i = i + 1
}