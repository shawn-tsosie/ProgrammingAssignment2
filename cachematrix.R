## Uses lexical scoping to cache the inverse of a matrix.

## Makes a list of four functions to take advantage of lexical scoping.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Determines whether or not the inverse has been previously solved.
## If it has been previously solved, it returns the cached version.
## If it has not been previously solved, it calculates the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
