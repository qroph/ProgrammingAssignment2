## These functions are used to cache the inverse of a matrix.

## A function to generate a list of functions that are used to store
## and retrieve a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # A cached inverse
    inverse <- NULL
    
    # Sets a new matrix and clears the previously cached inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Returns the matrix
    get <- function() x
    
    # Sets the inverse of the matrix
    setinverse <- function(i) inverse <<- i
    
    # Returns the inverse of the matrix
    getinverse <- function() inverse
    
    # Generate the list of functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## A function that returns a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
    # Get a cached inverse
    inverse <- x$getinverse()
    
    # Return the cached inverse if it exists
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # Otherwise calculate the inverse and cache it for later use
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    # Return the calculated inverse
    inverse
}
