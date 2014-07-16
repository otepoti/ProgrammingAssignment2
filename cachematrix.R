## The functions below provide a way of caching a matrix along with its inverse 
## so that the inverse doesn't have to be recalculated every time it's needed.

## makeCacheMatrix takes a matrix object as an argument and caches it.
## It then returns a list of functions for setting and getting the cached matrix
## together with functions for setting and getting its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve takes as an argument a list created by makeCacheMatrix.
## It then returns the inverse of the cached matrix. If the inverse has been  
## calculated before, it retrieves the cached version, otherwise it calculates
## it using solve(), caches it before returning the result.

cacheSolve <-function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    matrx <- x$get()
    inv <- solve(matrx, ...)
    x$setinverse(inv)
    inv
}
