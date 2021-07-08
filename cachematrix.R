
## a special matrix object that 
## caches the inverse of a matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
## value of the matrix
  get <- function() {x}
## set the value of the inverse
  setInverse <- function(inverse) {m <<- inverse}
## get the value of the inverse
  getInverse <- function()m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("currently getting cached data...wait...")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}
