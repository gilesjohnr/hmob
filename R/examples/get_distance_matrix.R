# Some XY coords in decimal degrees
xy <- data.frame(x=rnorm(10, 100, 1), 
                 y=rnorm(10, 20, 1),
                 id=LETTERS[1:10])

D <- get.distance.matrix(x=xy[,1],
                         y=xy[,2],
                         id=xy[,3])

see(D)
see(D*111.35) # in km
