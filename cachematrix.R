## This file contains 2 funtions makeCacheMatrix & cacheSolve
## makeCacheMatrix creates a Matrix, its inverse, and caches its inverse
## this assumes the Matrix will always be invertable
## the Matrix will contain all values squared, the inverse will contain all square roots

## this function makeCacheMatrix creates a Matrix, all of the values are squared (invertable), 
## and caches the value in the global environment for use by the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  makeMatrix <- function(solve) m<<- solve
  replyMatrix <- function() m
  list(make = make, reply = reply,
       makeMatrix = makeMatrix,
       replyMatrix = replyMatrix)
}


## This function returns the inverse of the original Matrix from makeCacheMatrix
## and if it has changed it updates the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$replyMatrix()
  if (!is.null(m)) {
    message("retrieving cached data")
    return(m)
  }
  matrix <- x$make()
  m <- solve(matrix, ...)
  x$makeMatrix(m)
  m
}
