## The following script has two functions: makeCacheMatrix and cacheSolve. 
## Together the two functions cache the inverse of a matrix.

## This function creates a list of functions to set the matrix, get the matrix,
## set the inverse of the matrix and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function returns the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse of the matrix is already caculated, the function will return the cached 
## inverse data. If not, the function will calculate and return the inverse with the solve 
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
