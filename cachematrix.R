## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Cached inverse initialized to NULL
  
  set <- function(y) {
    x <<- y       # Store new matrix
    inv <<- NULL  # Reset cached inverse
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getInverse <- function() inv  # Return the cached inverse
  
  # Return list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: Computes the inverse of the special "matrix" object
## returned by makeCacheMatrix. If the inverse is already cached,
## it retrieves it instead of computing it again.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return cached inverse
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setInverse(inv)       # Cache the result
  inv                     # Return the inverse
}       
}
