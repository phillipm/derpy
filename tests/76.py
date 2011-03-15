def two_e(str):
  count = 0
  for ch in str:  ## this loops over each char in the string
    if ch == 'e':
      count = count + 1

  if count == 2:
    return True
  else:
    return False
  ## this last if/else can be written simply as "return (count == 2)"

