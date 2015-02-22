## This set of functions will create a special object that 
## stores a square matrix and caches the inverse of that matrix.

## This function creates the cacheable matrix object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a square matrix.
## If the inverse has already been calculated it will return
## the cached calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, y=diag(ncol(data)), ...)
  x$setinverse(m)
  m
}
