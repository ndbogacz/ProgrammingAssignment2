## The following functions will return the inverse of a matrix
## The design allows for caching of values, to save time
## The functions assume an inversible matrix as input

## makeCacheMatrix will create a list of functions to use in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will check to see if the inverse has already been calculated
## if so, it will return the cached value
## if not, it will calculate the inverse and cache that

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
