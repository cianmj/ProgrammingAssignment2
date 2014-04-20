#
## Matrix inversion is usually a costly computation and their may 
# be some benefit to caching the inverse of a matrix rather than 
# compute it repeatedly.
# The makeCacheMatrix function creates a special "matrix" object 
# that can cache its inverse.
# The cacheSolvefunction computes the inverse of the 
# special "matrix" or returns a cached result.
#
#
## The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to :
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
#
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    inv <<- inverse
  }
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}
#
## The second function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the 
# inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
# Computing the inverse of a square matrix is done with 
# the solve function. This returns a matrix that is the 
# inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}

