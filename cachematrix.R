## Functions to support cacheing of inverse matrix

## Create a special list of functions supporting caching the inverse of a matrix
## returns a list of functions
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the inverse of the matrix
## getinverse - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Caches the inverse matrix on first call and
## uses this cached value on subsequent calls w/ x
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
  
}
