## These functions together provide a solution for inverting matrices and extracting their inverted value
## Caching is used to prevent expensive recomputation of the inverse

## Create a list representing an environment that stores a cached inverted matrix
makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        x
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
