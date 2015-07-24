## These functions cache the inverse of a matrix to save time
## during repeat computations when the matrix has not changed.

## This function creates a special "matrix" object that 
## can cache its inverse. The "matrix" is really a list
## whose elements are functions that:

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initializes the value of inverse, i, as soon as the main
  ## function is called. If this is removed, cacheSolve() 
  ## throws an error.
  i <- NULL
  
  ## sets the value of x, clears inverse, i
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Returns x, which is the input to the makeCacheMatrix
  ## function.
  get <- function() x
  
  ## This section
  ## If you remove the <<- in the next line, then there
  ## is no caching of the inverse.
  setinverse <- function(solve) m <<- solve
  
  ## returns value i, which is the inverse of x
  getinverse <- function() i
  
  ## Returns a list of functions as output
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
