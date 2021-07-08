
## Creates a special matrix object that 
## can cache the inverse of a matrix

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
## value of the matrix
  get <- function() {x}
## set the value of the inverse
  setInerse <- function(inverse) {inv <<- inverse}
## get the value of the inverse
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
## Double assignment operator is useful in conjunction. They modify variables in parent levels.
#A function written by a function closest includes the parent function to have access to all the variables and parameters

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("currently getting cached data...wait...")
    return(inv)
  }
  mat <- x$get()
## to compute the inverse of a matrix solve is the standard R function
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
