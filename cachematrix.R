## The following functions are used to cache the inverse of a matrix.
## This helps avoid recalculating the inverse multiple times, which can
## be computationally expensive if the matrix is large.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions:
## 1. set()        -> sets the value of the matrix
## 2. get()        -> gets the value of the matrix
## 3. setinverse() -> caches the inverse of the matrix
## 4. getinverse() -> retrieves the cached inverse (if it exists)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it directly.
## Otherwise, it computes the inverse using solve(), stores it in the cache,
## and returns the computed inverse.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}