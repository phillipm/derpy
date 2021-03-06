from mx.TextTools import *

word_set = set(alphanumeric+white+'-')
quant_set = set(number+'kKmM')

item   = ( (None, AllInSet, newline_set, +1),               # 1
           (None, AllInSet, white_set, +1),                 # 2
           ('Prod', AllInSet, a2z_set, Fail),               # 3
           (None, AllInSet, white_set, Fail),               # 4
           ('Quant', AllInSet, quant_set, Fail),            # 5
           (None, WordEnd, '\n', -5) )                      # 6

buyers = ( ('Order', Table,                                 # 1
                  ( (None, WordEnd, '\n>> ', Fail),         # 1.1
                    ('Buyer', AllInSet, word_set, Fail),    # 1.2
                    ('Item', Table, item, MatchOk, +0) ),   # 1.3
                  Fail, +0), )

comments = ( ('Comment', Table,                             # 1
                  ( (None, Word, '\n*', Fail),              # 1.1
                    (None, WordEnd, '*\n', Fail),           # 1.2
                    (None, Skip, -1) ),                     # 1.3
                  +1, +2),
             (None, Skip, +1),                              # 2
             (None, EOF, Here, -2) )                        # 3

def unclaimed_ranges(tagtuple):
    starts = [0] + [tup[2] for tup in tagtuple[1]]
    stops = [tup[1] for tup in tagtuple[1]] + [tagtuple[2]]
    return zip(starts, stops)

def report2data(s):
    comtuple = tag(s, comments)
    taglist = comtuple[1]
    for beg,end in unclaimed_ranges(comtuple):
        taglist.extend(tag(s, buyers, beg, end)[1])
    taglist.sort(cmp)
    return taglist

if __name__=='__main__':
    import sys, pprint
    pprint.pprint(report2data(sys.stdin.read()))

