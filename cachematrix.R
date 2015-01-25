## Programming Assignment 2
## Functions calculate inverse of a matrix and cache the result

## makeCacheMatrix: create special vector that caches results of computations on matrices
## Input matrix is x
## Returns vector of functions to set/get the matrix and set/get the results of a computation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setres <- function(inv) m <<- inv
  getres <- function() m
  list(set = set, get = get, 
       setres = setres, 
       getres = getres)
}


## cacheSolve: Calculate inverse of 'x' constructed with makeCacheMatrix
## Return cached result if exists, otherwise calculates and stores it.

cacheSolve <- function(x, ...) {
  m <- x$getres()
  if(!is.null(m)) {
      message("using cached data")
      return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setres(m)
  m
}
