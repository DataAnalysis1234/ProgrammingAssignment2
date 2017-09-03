## Put comments here that give an overall description of what your
## functions do

## creates a cached matrix, that can be used in cachesolve

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) invmat <<- inverse
  getInv <- function() invmat
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}


## Takes matrix created from makeCacheMatrix
## and calculates the inverse of said matrix.
## Additionally, it stores the value to save time
## if you re-run it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInv()
  if(!is.null(invmat)) {
    message("pulling calculated data")
    return(invmat)
  }
  data <- x$get()
  invmat  <- solve(data,...)
  x$setInv(invmat)
  invmat
}
