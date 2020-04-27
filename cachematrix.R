## The following functions work together to calculate, cache and recover the
## inverse of a given matrix, if it is already calculated and cached

## Returns a list containing functions to set and get the value of a matrix
## and set and get its inverse
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Calculates the inverse of the matrix returned by makeCacheMatrix function,
## or returns it from the cache if it is already calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
