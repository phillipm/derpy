### Comparison functions
lt = __lt__ = lambda a,b: a < b
le = __le__ = lambda a,b: a <= b
eq = __eq__ = lambda a,b: a == b
ne = __ne__ = lambda a,b: a != b
ge = __ge__ = lambda a,b: a >= b
gt = __gt__ = lambda a,b: a > b
### Boolean functions
not_ = __not__ = lambda o: not o
truth = lambda o: not not o
# Arithmetic functions
abs = __abs__ = abs   # same as built-in function
add = __add__ = lambda a,b: a + b
and_ = __and__ = lambda a,b: a & b  # bitwise, not boolean
div = __div__ = \
      lambda a,b: a/b  # depends on __future__.division
floordiv = __floordiv__ = lambda a,b: a/b # Only for 2.2+
inv = invert = __inv__ = __invert__ = lambda o: ~o
lshift = __lshift__ = lambda a,b: a << b
rshift = __rshift__ = lambda a,b: a >> b
mod = __mod__ = lambda a,b: a % b
mul = __mul__ = lambda a,b: a * b
neg = __neg__ = lambda o: -o
or_ = __or__ = lambda a,b: a | b    # bitwise, not boolean
pos = __pos__ = lambda o: +o # identity for numbers
sub = __sub__ = lambda a,b: a - b
truediv = __truediv__ = lambda a,b: 1.0*a/b # New in 2.2+
xor = __xor__ = lambda a,b: a ^ b
### Sequence functions (note overloaded syntactic operators)
concat = __concat__ = add
contains = __contains__ = lambda a,b: b in a
countOf = lambda seq,a: len([x for x in seq if x==a])
def delitem(seq,a): del seq[a]
__delitem__ = delitem
def delslice(seq,b,e): del seq[b:e]
__delslice__ = delslice
getitem = __getitem__ = lambda seq,i: seq[i]
getslice = __getslice__ = lambda seq,b,e: seq[b:e]
indexOf = lambda seq,o: seq.index(o)
repeat = __repeat__ = mul
def setitem(seq,i,v): seq[i] = v
__setitem__ = setitem
def setslice(seq,b,e,v): seq[b:e] = v
__setslice__ = setslice
### Functionality functions (not implemented here)
# The precise interfaces required to pass the below tests
#     are ill-defined, and might vary at limit-cases between
#     Python versions and custom data types.
import operator
isCallable = callable     # just use built-in 'callable()'
isMappingType = operator.isMappingType
isNumberType = operator.isNumberType
isSequenceType = operator.isSequenceType
