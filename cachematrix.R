## These functions allow user to pull from a cache if the calculation has already been done.

##makeCacheMatrix:  This function puts results into the cache if appropriate, and sets up functions to be used for retrieval by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()  
  setmat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmat()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
