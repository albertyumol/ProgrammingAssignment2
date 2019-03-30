## This code contains a pair of functions named makeCacheMatrix and cacheSolve 
## that cache the inverse of a matrix.

## An assumption for this assignment is that the input 'x' is a square invertible matrix,
## such that 'x' will always have an inverse.

## makeCacheMatrix creates a special 'matrix' object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## first we initialize the matrix 
  set <- function(y) { 
    x <<- y ## then we assign a value y to the matrix function
    i <<- NULL ## to avoid multiple counting, we clear the cache function
  }
  get <- function() x ## we get the value of the resulting matrix
  setinverse <- function(inverse) i <<- inverse ## and set a corresponding 'contatiner function for the inverse
  getinverse <- function() i ## we then get this the value in this 'container function'
  list(set = set, get = get, ## and list all the values
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## we then retrieve the value from the cache function above
  if(!is.null(i)) { ## if the cache has a value
    message("getting cached data") ## we retrieve it with this message
    return(i) ## and return it to be appended if needed
  }
  data <- x$get() ## if the cache doesn't have a value, we retrieve the original set value
  i <- solve(data, ...) ## solve its inverse
  x$setinverse(i) ## and apppend the result to the cache
  i ## finally, we print the resulting inverse of the matrix
}
