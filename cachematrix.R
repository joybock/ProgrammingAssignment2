## Functions that cache the inverse of a Matrix
## Demonstrates how results can be cached and retrieved for later user outside a function

## create a list of that can be used to get or set a matrix and then get or set it's inverse
## note that the inverse matrix is actually created in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## given a list created by makeCacheMatrix, test if the inverse a matrix exists
## create the inverse of the matrix if it doesn't exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
