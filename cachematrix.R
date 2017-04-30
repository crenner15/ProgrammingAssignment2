## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to do the following:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)    
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

#TEST RUN
#x = rbind(c(1, -1/4), c(-1/4, 1))
#m = makeCacheMatrix(x)
#m$get()
## Second run returned cached data
#cacheSolve(m)