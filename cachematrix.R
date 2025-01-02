# These functions are designed to cache the inverse of a matrix to avoid 
# re computation when the same matrix is used multiple times. The first 
# function creates a special matrix object that can store its inverse, 
# and the second function computes the inverse, using the cached value 
# if it has already been calculated.

## 1. This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y  # Assign the new matrix to x in the parent environment
    inv <<- NULL  # Reset the inverse cache
  }
  # Function to get the value of the matrix
  get <- function() {
    x
  }
  # Function to set the value of the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the value of the inverse
  getInverse <- function() {
    inv
  }
  # Return a list of functions to interact with the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 2. This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve retrieves it from 
# the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get cached inverse
  
  # Check if the inverse is already calculated
  if (!is.null(inv)) {
    message("getting cached data")  # Print message if using cached data
    return(inv)  # Return cached inverse
  }
  
  # If not cached, compute the inverse
  data <- x$get()  # Get the original matrix
  inv <- solve(data, ...)  # Calculate its inverse
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the computed inverse
}
        
