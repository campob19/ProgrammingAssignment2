## The makeCacheMatrix function takes a matrix and outputs a list of four functions
## the cacheSolve function takes a invertable matrix and inverts the matrix using the cached functions
## from the makeCacheMatrix function.

## the makeCacheMatrix function outputs a list of four functions that will be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## the cacheSolve function finds the inverse of a matrix using the four functions.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
