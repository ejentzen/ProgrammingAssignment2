## These functions work together to cache the inverse of a matrix, aiming to optimize
## performance for expensive matrix inversion operations. The makeCacheMatrix function 
## creates a list of closures around a matrix, providing functionalities to set the matrix, 
## get the matrix, set the cached inverse, and get the cached inverse. The cacheSolve 
## function computes the inverse of the matrix, checks for a cached version, and returns 
## the inverse.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix and its inverse.

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


## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## It first checks if the inverse is already cached. If it is, it retrieves it from the cache.
## Otherwise, it calculates the inverse, stores it in the cache, and then returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
