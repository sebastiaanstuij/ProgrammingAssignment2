## This file contains 2 functions "makeCacheMatrix" and "cacheSolve"
## these functions are used to create a 'cached' matrix which is especially usefull
## for computations (in this case calculating the inverse) on large matrices

## "makeCacheMatrix" creates a 'cached' version of a matrix with is actually a list 
## containint the original matrix and several functions for storing cached values
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setSolve <- function(solveMatrix = matrix()) m <<- solveMatrix
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
   
}


## "cacheSolve" is an alternative to the standard solve() function which is used to
##  calculate the inverse of a matrix, in which this function first looks whether this
## has already been computed (cached) and then returns the cached result if possible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setSolve(m)
    m
  
}
