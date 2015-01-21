## These functions implement a solution to solve for the inverse of a matrix,
## with caching.  The function cacheSolve must be passed a list from
## makeCacheMatrix, which then checks the cache to see if the inverse has
## already been calculated and either calculates and returns the inverse or
## returns the cached matrix.

## makeCacheMatrix must be passed an invertible matrix and returns a list of
## functions that can be used to check and set the cache for the inverse of
## the parameter matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  return(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


## cacheSolve calculates the inverse of a matrix, but first checks to its cache
## to see if it's already been calculated and either fetches the cached
## solution or calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  return(i)
}