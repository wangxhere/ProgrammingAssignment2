## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. These two functions do such work as an example.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # buffer for inverse
  inverse <- NULL
  # set data content
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get data content
  get <- function() x
  # set inverse cache
  setinverse <- function(inverse) inverse <<- inverse
  # get value of inverse cae
  getinverse <- function() inverse
  # output
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  ## return cache if exists and valid
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # get data content
  data <- x$get()
  # do inverse
  inverse <- solve(data)%*%data
  # set inverse to cache
  x$setinverse(inverse)
  # output
  inverse
}
