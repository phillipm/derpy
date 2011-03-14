class Visitor(Object):

    def __init__(self):
        super(Visitor, self).__init__()

    def visit(self, obj):
        pass
    
    def getIsDone(self):
        return False

    isDone = property(
        fget = lambda self: self.getIsDone())

