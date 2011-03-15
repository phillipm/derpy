shape = int(input(menu))
while shape != 4:
   if shape == 1:
      length = float(input("Length: "))
      print( "Area of square = ", length ** 2 )
   elif shape == 2:
      length = float(input("Length: "))
      width = float(input("Width: "))
      print( "Area of rectangle = ", length * width )  
   else:
      print(" Not a valid shape. try again")

