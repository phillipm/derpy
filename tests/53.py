class Complex(object):

    def main(*argv):
        "Complex test program."
        c = Complex(0, 0)
        print c
        c.real = 1
        c.imag = 2
        print c
        c.theta = math.pi/2 
        c.r = 50
        print c
        c = Complex(1, 2)
        d = Complex(3, 4)
        print c, d, c+d, c-d, c*d
        return 0
    main = staticmethod(main)

