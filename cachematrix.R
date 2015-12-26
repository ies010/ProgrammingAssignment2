## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. Below are a pair of functions 
## that are used to create a special object that stores a matrix and caches its inverse.

# Creates a special "matrix" object to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Computes the inverse of "matrix" created by makeCacheMatrix. If the inverse has 
## already been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInverse()
    if (!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    mat <- x$get()
    invMatrix <- solve(mat, ...)
    x$setInverse(invMatrix)
    invMatrix
}
