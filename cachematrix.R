## This pair of functions is supposed to cache the inverse of a matrix.

## The first function, called makeCacheMatrix, creates a "matrix" object, which contains
## a function to set the value of the matrix, to get the value, to set the inverse and to get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## The second function, cacheSolve, computes the inverse of the matrix object which was returned by the first function. 
## If the inverse has already been computed, it skips the computation and the value is retrieved from the cache. 
## Otherwise it computes the inverse and sets this value into the cache by means of the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
