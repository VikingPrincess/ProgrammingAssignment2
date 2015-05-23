## Here's a pair of functions that cache the inverse of a matrix.
## The goal here is to learn to cache potentially time-consuming computations.

## Function makeCacheMatrix sets value of the vector X, assigns it to mtrx,
## then gets the value of the vector and returns what is actually a list that caches the matrix

makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtrx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtrx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function will take over value mtrx from makeCacheMatrix and calculate the inverse
## matrix using solve()

cacheSolve <- function(mtrx, ...) {
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtrx$get()
  invserse <- solve(data, ...)
  mtrx$setinv(inverse)
  return(inverse)
        ## Return a matrix that is the inverse of 'x'
}

