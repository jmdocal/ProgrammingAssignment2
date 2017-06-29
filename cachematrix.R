##################################################
# Matrix Caching  -  R language course
#   
#   José M. Hernández
#   Programming Test # 2
#   June 2017
# 
# This source file constaints two functions 
# makeCacheMatrix and cacheSolve
# The first one has the mission of storing the
# matrix provided by the user and its inverse
# if has been calculated
# 
# The cacheSolve function take the matrix 
# provided by the user and storaged using
# the makeCacheMatrix function, then if it's
# the first time this matrix is used, cacheSolve
# calculates the inverse (if it's possible) and 
# storages it with the help of makeCacheMatrix
#
   

############################################################
# makeCacheMatrix - Provides an storage's space for allocating
#       a matrix type object and its inverse
# subfunctions calls:
#   $set(x): Allocates the matrix provided as input arg to 
#         the cache storage
#   $get: Returns the matrix previously storaged
#   $setinv(x): Allocates the matrix provided as input arg
#         to a cache storage reserved for the inversed 
#         matrix set by $set(x), it doesn't do the inverse, 
#         only storages a matrix within the space allocated
#         for that purpose.
#   $getinv: Returns de matrix storaged into the cache reserved
#         for the inverse
  

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y=matrix()) {
    x <<- y
    x_inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(y = matrix()) {
    x_inv <<- y
  }
  
  getinv <- function() x_inv
  
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


############################################################
# cacheSolve - Obtain the inverse of an special cached matrix
# provides as input argument
# input: x - Special cached martrix created with makeCacheMatrix
# return: Inverse of x. If the inverse matrix was calculated 
#       previously the function returns the storaged value
#       not being necessary to calculate it again
# 

cacheSolve <- function(x, ...) {
  x_inv <- x$getinv()
  if (!is.null(x_inv)) {
    # message("result obtained from cache")
    return(x_inv)
    
  }
  xmain<-x$get()
  x_inv<-solve(xmain)
  x$setinv(x_inv)
  x_inv
}
