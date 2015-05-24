## This file contains functions for faster matrix inverion by
## making use cached values for inverses, where possible

## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  stored_inv <- NULL
  get <- function() {
    x
  }
  setInverse <- function(new_inv) {
    stored_inv <<- new_inv
  }
  getInverse <- function() {
    stored_inv
  }
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" x
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  stored_inv <- x$getInverse()
  if(is.null(stored_inv)) {
    data <- x$get()
    new_inv <- solve(data, ...) # calculate the inverse
    x$setInverse(new_inv)
    stored_inv <- new_inv
  }
  else {
    message("Getting cached data...")
  }
  return(stored_inv)
}
