# Treat a GZ as "just another file"
import gzip, glob
print "Size of data in files:"
for fname in glob.glob('*'):
    try:
        if fname[-3:] == '.gz':
            s = gzip.open(fname).read()
        else:
            s = open(fname).read()
        print ' ',fname,'-',len(s),'bytes'
    except IOError:
        print 'Skipping',file

