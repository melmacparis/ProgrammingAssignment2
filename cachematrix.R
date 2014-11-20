## The function makeCacheMatrix creates a special "matrix" whic 
## sets and gets the matrix value and sets and gets the inverse of 
## the matrix

## The function makeCacheMatrix creates a matrix and sets and gets its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" in 
## makeCacheMatrix.  It first checks the cache for the inverse, 
## otherwise, it computes the inverse and sets it as the new value
## for the inverse in the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
