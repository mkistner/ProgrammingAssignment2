## This file contains functions for faster matrix inverion by
## making use cached values for inverses, where possible

## makeCacheMatrix makes three functions available: get, setInverse
## and getInverse. These are used from within cacheSolve to 
## compute the inverse of matrix x, using a cached inverse if it 
## has been computed before.

makeCacheMatrix <- function(x = matrix()) {
  stored_inv <- NULL
  # simply return the matrix
  get <- function() { 
    x
  }
  # store the matrix new_inv so that it may be accessed from cache
  setInverse <- function(new_inv) {
    stored_inv <<- new_inv
  }
  # return the stored inverse
  getInverse <- function() {
    stored_inv
  }
  # return the 3 functions that were defined
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" x
## returned by makeCacheMatrix (the input for cacheSolve is the
## output from makeCacheMatrix). cacheSolve uses the cached 
## inverse instead of computing it again, if it has been computed 
## before.

cacheSolve <- function(x, ...) {
  # get the stored inverse (NULL if it has not been computed before)
  stored_inv <- x$getInverse()
  # if it is NULL (not computed before)
  if(is.null(stored_inv)) {
    data <- x$get() # get the matrix x
    new_inv <- solve(data, ...) # calculate its inverse
    x$setInverse(new_inv) # store its inverse
    stored_inv <- new_inv
  }
  else { # the inverse already exists in 'stored_inv'
    message("Getting cached data...")
  }
  return(stored_inv)
}
