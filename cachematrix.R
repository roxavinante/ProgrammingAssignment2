## by Roxanne S. Avinante, Dec 03, 2017
## Programming Assignment 2 - R Programming
##
## Caching the Inverse of a Matrix


## ================== makeCacheMatrix ==================
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  
  ## b. get the value of the matrix
  get <- function() 
    x
  
  ## set the value of inverse of the matrix
  setinverse <- function(inverse_y) inverse_x <<- inverse_y
  
  ## get the value of inverse of the matrix
  getinverse <- function() 
    inverse_x
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## ================== cacheSolve ==================
## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),  
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data...")
    inverse
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
