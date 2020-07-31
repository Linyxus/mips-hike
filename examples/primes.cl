var i, t, k;
mem vis[] = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    len = 30;

i = 2
while i < len {
  t = *(&vis + i * 4)
  if t {
    k = 2
    while i * k < len {
      &vis + i * k * 4 .= 0
      k = k + 1
    }
  }
  i = i + 1
}

i = 2
while i < len {
  t = *(&vis + i * 4)
  if t {
    print i
  }
  i = i + 1
}