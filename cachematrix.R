## The following functions are used to compute the inverse of a matrix and cache the result for later use.

## This function acts as a constructor for creating a "special matrix" object that is able to cache its inverse after being computed.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(m) inverse <<- m
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieves the inverse of a "special matrix" object. If it is already calculated, then it serves the result from cache instead of repeating the computation.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
