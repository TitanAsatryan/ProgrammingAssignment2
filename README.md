## This function creates a special "matrix" object that can cache its inverse.
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

## The followeing function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  
  inv
}
