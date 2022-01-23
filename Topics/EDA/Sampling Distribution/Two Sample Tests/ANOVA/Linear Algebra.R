####BAX Foundation 400####
####Linear Algebra Vectors
##Exercise 1- Vector Operations
u <- c(2, -7, 1)
v <- c(-3, 0, 4)
w <- c(0, 5, -8)

##a
3*u - 4*v
2*u + 3*v - 5*w

##Exercise 2 - Norm of Vectors
u <- c(3.2, -0.6, -1.4)
norm(as.matrix(u), "F")

v <- c(1.5, 4.1, -0.2)
norm(as.matrix(v), "F")

##Exercise 3 - Normalize the vector

u <- c(3.2, -0.6, -1.4)
l_u <- norm(as.matrix(u), "F")
u/l_u

v <- c(1.5, 4.1, -0.2)
l_v <- norm(as.matrix(v), "F")
v/l_v

##Exercise 4 - Distance between two vectors

u <- c(3.2, -0.6, -1.4)
v <- c(1.5, 4.1, -0.2)

u-v
norm(as.matrix(u-v), "F")

##Exercise 5 - Dot Product of Two Vectors
u <- c(2, -1, 1)
v <- c(1, -2, -1)

crossprod(u,v)

u <- c(3,0)
v <- c(-1, 1)

crossprod(u,v)

##Exercise 6 - Angle between two vectors

u <- c(2, -1, 1)
v <- c(1, -2, -1)

l_u <- norm(as.matrix(u), "F")
l_v <- norm(as.matrix(v), "F")

dot_prod <- crossprod(u,v)

theta <- acos(dot_prod/(l_u * l_v))
as.numeric(theta)

theta*180/pi

u <- c(3, 0)
v <- c(-1, 1)

l_u <- norm(as.matrix(u), "F")
l_v <- norm(as.matrix(v), "F")

dot_prod <- crossprod(u,v)

theta <- acos(dot_prod/(l_u * l_v))
as.numeric(theta)

theta*180/pi

##Exercise 7 - Orthogonality of Vectors

u <- c(1, -2, 3)
v <- c(4, 5, -1)
w <- c(2, 7, 4)

cross_prod1 <- crossprod(u,v)
cross_prod2 <- crossprod(u,w)

if(cross_prod1 == 0){
  cat("u and v are Orthogonal") 
  } else {
  cat("u and v are NOT Orthogonal") 
  }

    
if(cross_prod2 == 0){
  cat("u and w are Orthogonal") 
} else {
  cat("u and w are NOT Orthogonal") 
}

##Excercise 8 - Vector Projections

v < c(-1, 1)
u <- c(-2, 4)

dot_prod_uv <- crossprod(u, v)
dot_prod_uu <- crossprod(u, u)

ratio <- (dot_prod_uv)/dot_prod_uu

as.matrix(as.vector(ratio)*u)

##Linear Algebra Homework

##Question 1
 
u <- c(2, -7, 1)
v <- c(-3, 0, 4)
w <- c(0, 5, -8)

3*u - 4*v
2*u + 3*v - 5*w

##Question 2:
u < c(1, 2, 3)
v <- c(3, 2, 1)

norm(as.matrix(u), "F")
norm(as.matrix(v), "F")

##Question 3: Unit vectors

u <- c(1, 2, 3)
v <- c(3, 2, 1)


v/(sqrt(sum(v*2,na.rm=TRUE)))
u/(sqrt(sum(u*2,na.rm=TRUE)))

## Question 4: Distance 

u <- c(1, 2, 3)
v <- c(3, 2, 1)

u-v
norm(as.matrix(u-v), "F")

##Question 5 - Dot Product of Two Vectors
u <- c(4, 3, -1)
v <- c(1, -1, 1)

crossprod(u,v)

u <- c(-1,2)
v <- c(3, 1)

crossprod(u,v)

##Question 6 - Angle between two vectors

u <- c(4, 3, -1)
v <- c(1, -1, 1)

l_u <- norm(as.matrix(u), "F")
l_v <- norm(as.matrix(v), "F")

dot_prod <- crossprod(u,v)

theta <- acos(dot_prod/(l_u * l_v))
as.numeric(theta)

theta*180/pi

u <- c(-1, 2)
v <- c(3, 1)

l_u <- norm(as.matrix(u), "F")
l_v <- norm(as.matrix(v), "F")

dot_prod <- crossprod(u,v)

theta <- acos(dot_prod/(l_u * l_v))
as.numeric(theta)

theta*180/pi

## Question 7: Orthogonal
  

u <- c(2, 3, -4)
v <- c(3, -1, -2)


cross_prod1 <- crossprod(u,v)
cross_prod2 <- crossprod(u,w)

if(cross_prod1 == 0){
  cat("u and v are Orthogonal") 
} else {
  cat("u and v are NOT Orthogonal") 
}

## Question 8: Projection

v < c(3/5, -4/5)
u <- c(1,2)

dot_prod_uv <- crossprod(u, v)
dot_prod_uu <- crossprod(u, u)

ratio <- (dot_prod_uv)/dot_prod_uu

as.matrix(as.vector(ratio)*u)





