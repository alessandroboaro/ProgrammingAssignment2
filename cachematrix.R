## makeCacheMatrix create an object able to cache a matrix and its inverse
## cacheSolve operates on a makeCacheMatrix object and actually compute the inverse of a matrix contained in. Caching the result


## this function creates an object containing a matrix and its inverse and
## provide all methods to interact with the two objects

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks if an inverse was already calculated and in case return it.
## Otherwise, compute the inversion and cache the result in x object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


