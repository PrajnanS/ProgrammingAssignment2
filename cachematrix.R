# This function calculates Matrix Inverse in optimized way. If inverse is already calculated
# the function will not recalculate but retrieve the same from cache

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
Inverse<-NULL
set<-function(y){
  x<<-y
  Inverse<<-NULL
}
get<-function() x
setInverse<-function(solve) Inverse<<-solve
getInverse<-function() Inverse
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
Inverse<-x$getInverse()
if (!is.null(Inverse)) {
  message("getting cached data")
  return(Inverse)
}
data <- x$get()
Inverse <- solve(data, ...)
x$setInverse(Inverse)
Inverse
}
