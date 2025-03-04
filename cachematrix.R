makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then it retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse
  
  # Return cached inverse if available
  if (!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}
