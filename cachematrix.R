## Cache the inverse of a matrix to reduce computational time
## Includes makeCacheMatrix and cacheSolve functions 

## This makeCacheMatrix function will create a special matrix that caches its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    x <<- y    
    inv <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
}


## The cacheSolve function computes the inverse of the special matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  matrix_invert <- x$get()
  inv <- solve(matrix_invert, ...)
  x$setinverse(inv)
  inv
}
