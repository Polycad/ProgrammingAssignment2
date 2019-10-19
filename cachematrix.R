## Matrix inversion is a costly operation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This code make the pairs of functions that cache the inverse of a matrix.

## The function "makeCacheMatrix" is used for creating "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    p <- NULL
  set <- function(y) {
          x <<- y
          p <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function "cacheSolve" computes the inverse of the returned "matrix". 
## Previosly calculated inverse of the "matrix" should be retrieve from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  p <- x$getinverse()
  if (!is.null(p)) {
          message("getting cached data")
          return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}
