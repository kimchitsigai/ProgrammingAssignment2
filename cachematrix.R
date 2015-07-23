######################################################
## makeCacheMatrix is a function object which is used 
## to cache a matrix and its inverse.
## get() returns the current value of the matrix
## set() sets the value of the matrix
## getinv() returns the current inverse of the matrix
## setinv() sets the inverse of the matrix
######################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  
  setinv <- function(v) inv <<- v 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#######################################################################
## cacheSolve() returns the inverse of a matrix.
## Its input argument is a cache object created with makeCacheMatrix().
## If the inverse exists in the cache, it is simply returned.
## Otherwise, the matrix is read from the cache, 
## its inverse is computed with solve(), cached and returned.
#######################################################################
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}