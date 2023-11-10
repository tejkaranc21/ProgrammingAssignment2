## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
   ## Initialize the matrix
   data <- mat
   
   ## Initialize the cached inverse (start as NULL since it hasn't been calculated yet)
   cached_inverse <- NULL
   
   ## Function to set the matrix
   set <- function(new_matrix) {
      data <<- new_matrix
      cached_inverse <<- NULL  # Reset the cached inverse when the matrix changes
   }
   
   ## Function to get the matrix
   get <- function() data
   
   ## Function to set the cached inverse
   setInverse <- function(inverse) {
      cached_inverse <<- inverse
   }
   
   ## Function to get the cached inverse
   getInverse <- function() cached_inverse
   
   ## Return a list of functions
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to compute the inverse of the special "matrix" object
## If the inverse has already been calculated and the matrix has not changed,
## retrieve the inverse from the cache

cacheSolve <- function(cacheMatrix, ...) {
   cached_inverse <- cacheMatrix$getInverse()
   
   ## If the cached inverse exists, return it
   if (!is.null(cached_inverse)) {
      message("getting cached data")
      return(cached_inverse)
   }
   
   ## If the cached inverse doesn't exist, calculate it
   data <- cacheMatrix$get()
   inverse <- solve(data, ...)
   
   ## Cache the calculated inverse
   cacheMatrix$setInverse(inverse)
   
   ## Return the calculated inverse
   inverse
}
