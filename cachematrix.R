## Matrix inversion is usually a costly computation, and there may be
## some benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. These functions will allow us to compute the inverse of a matrix 
## and store it in cache so the next time we need the matrix inverse we can 
## simply get it from cache instead of recomputing it.


## The makeCacheMatrix function creates a special "matrix", which is really just
## a list containing 4 functions:
## set: sets the value of the matrix
## get: returns the value of the matrix
## setInverse: sets the value of the inverse matrix
## getInverse: gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse and 
## sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

