## Functions to calculate the inverse of an invertible matrix and cache the
## result in a cache object.

## Create a cache object which stores a matrix and its inverse (inv), plus
## associated functions to set and get them.

makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(y) inv <<- y
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Invert the matrix in the given cache object (x) and store it in the object.
## If the object already contains data, return the cached data instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
