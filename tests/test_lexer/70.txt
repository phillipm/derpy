def make_counter(x):
  print('entering make_counter')
  while True:
    yield x                  
    print('incrementing x')
    x = x + 1

