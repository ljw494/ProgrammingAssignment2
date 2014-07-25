## These functions calculate and cache the inverse of a provided matrix.

## The first function sets and gets the matrix provided, and sets and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse of the matrix has already been calculated, 
## and calculates it if it has not been calculated previously.

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
