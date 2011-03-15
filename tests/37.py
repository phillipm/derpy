from itertools import takewhile

def fib():
    x,y = 1,1
    while True :
        x,y = y,x+y
        yield x

