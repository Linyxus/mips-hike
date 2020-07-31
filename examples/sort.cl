var k, i, x, y, t;
mem a[] = [132, 42, 31, 91, 1, 2],
    len = 6;

// print out the array before sorting
i = 0
while i < len {
  print *(&a + i * 4)
  i = i + 1
}

// sort the array
k = 0
while k < len {
  i = 0
  while i < len - 1 {
    x = *(&a + i * 4)
    y = *(&a + (i + 1) * 4)
    if y < x {
      t = x
      x = y
      y = t
    }
    &a + i * 4 .= x
    &a + (i + 1) * 4 .= y
    i = i + 1
  }
  k = k + 1
}

// print out the sorted array
i = 0
while i < len {
  print *(&a + i * 4)
  i = i + 1
}