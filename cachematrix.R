## Assignment 2
## Write a pair of functions that cache the inverse of a matrix.
## by Writing the following functions:
## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.
## Function 1:
## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.
## code follows
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 
}


## Write a short comment describing this function
## function 2: cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve would retrieve the inverse from the cache.
## code follows
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Found cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
        
}
