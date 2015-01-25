## Programming Assignment 2
## Functions to calculate inverse of a matrix and cache the result
## Sample usage: 
## cachingMatrix = makeCacheMatrix(myMatrixToInvert)
## inverse = cacheSolve(cachingMatrix)

## makeCacheMatrix: create special vector that caches results of computations on matrices
## Stores matrix given on input (or empty if not given)
## Returns vector of functions to set/get input matrix and set/get the cached result 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # m stores result if already calculated
  set <- function(y) {   # store the input matrix
      x <<- y
      m <<- NULL #  we have a new matrix so reset any cached results
  }
  get <- function() x  # return the stored input matrix
  setres <- function(inv) m <<- inv  # store the result in cache
  getres <- function() m   # return the cached result
  
  list(set = set, get = get, 
       setres = setres, 
       getres = getres)
}


## cacheSolve: Calculate inverse of 'x' constructed by makeCacheMatrix
## Return cached result if exists, otherwise calculates and stores it.

cacheSolve <- function(x, ...) {
  m <- x$getres()
  if(!is.null(m)) { 
      # result was cached already
      message("using cached data")
      return(m)
  }
  
  # result wasn't cached so compute and store 
  data <- x$get()
  m <- solve(data, ...)
  x$setres(m)
  m
}
