## Put comments here that give an overall description of what your
## functions do

## Create a matrix that caches computation of the inverse of the matrix.
## The returned list will have the following functions as components for
## access:
##
## - get(): get the underlying matrix
## - set(y): set the underlying matrix to `y`, this will clear the inverse cache
## - get.inverse(): get the cached inverse
## - set.inverse(inverse) -- set the cached inverse to `inverse`
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) i <<- inverse
  get.inverse <- function() i
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Solve the matrix using `solve()` and cache the inverse.
## Subsequent calls will returned the cached value.
cacheSolve <- function(x, ...) {
  i <- x$get.inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set.inverse(i)
  i
}
