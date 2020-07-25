makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inv)
  inverse      
}