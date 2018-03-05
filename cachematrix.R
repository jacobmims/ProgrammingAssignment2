## This script file will allow a user to create a matrix object capable of caching its inverse
## and and solving for said inverse.

## Creates a cached matrix for any given input. Call this first.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(invertMatrix) m <<- invertMatrix
    getInverse <- function() m
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Solves an inverse matrix utilizing a cache for repeat requests. 
## Use the output of makeCacheMatrix for 'x'
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Cached inverse found! Retrieving...")
        return(m)
    }
    else {
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
    }
}
