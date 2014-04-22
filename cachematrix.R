## Put comments here that give an overall description of what your
## functions do

## This function should create a special object which stores a matrix and
## and can, also, store its inverse. (It will be stored with the name inverse)
makeCacheMatrix <- function(x = matrix()) {
  ## inverse is set to null when creating
  inverse <- NULL 
  ## This function sets the value of x(matrix) and resets the inverse value to null
  ## so it has to be recalculated. It is used to change the x value.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get: returns the matrix. x
  get <- function() x
  ## setinverse: stores the inverse value.
  setinverse <- function(inv) inverse <<- inv
  ## getinverse: returns the inverse value.
  getinverse <- function() inverse
  ## The return objet is a list ofavailable functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns calculated the inverse of the matrix stored using
## makeCachematrix function. It is done in two steps. First it checks if the 
## inverse value has been cached and it returns the stored cache value if exists.
## Otherwise, it computes the inverse value of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if inverse has been previously computed.
    inv <- x$getinverse()
    if(!is.null(inv)) {
      ## As inverse has been previously computed, we return the cached value.
      message("getting cached data")
      return(inv)
    }
    ## As there is not any cached value, we compute that.
    data <- x$get()
    inv <- solve(data, ...)
    ## Before returning the inverse value, we cache it for future calls.
    x$setinverse(inv)
    inv    
}
