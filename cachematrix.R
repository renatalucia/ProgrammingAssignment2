## This file contains two functions:
##    makeCacheMatrix: Creates a matrix that can cache its inverse,
##    cacheSolve: Computes and caches the inverse of the matrix, or retrieves the cached inverse.

## This function creates a special "matrix" object that can cache its inverse
## The special matrix is list containing functions to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes or retrieves the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has been calculated previvously and the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache,
## otherwise cachesolve computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
