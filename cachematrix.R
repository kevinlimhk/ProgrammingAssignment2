## Function - makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## Variables:
## x -> matrix
## m -> inverse of matrix
## It contains the following methods:
## set(): sets matrix and reset cached inverse
## get(): returns the matrix
## setInverse: saves the inverse
## getInverse: returns the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set() method
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get() method
    get <- function() 
        {
        x
    }
    # setInverse() method
    setInverse <- function(solve) 
        {
        m <<- solve
    }
    # getInverse() method
    getInverse <- function() 
        {
        m
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function - cacheSolve
## This function computes and saves the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated, then the 
## cachesolve should retrieve the inverse from the cache.
## Variables:
## x -> matrix
## m -> inverse of matrix
cacheSolve <- function(x, ...) {
    # Get inverse from special "matrix"
    m <- x$getInverse()
    # Checks if inverse is not null
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If inverse is null, function continues to calculate and cache the inverse.
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
