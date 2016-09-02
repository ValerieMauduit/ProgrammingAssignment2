## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    ## Function that creates a special "matrix" object that can cache its 
    ## inverse
    ## Input:
    ##  - 'x' : classic matrix
    ## Output:
    ##  - list contening the 'x' matrix and its inversed, if yet calculated
    ##      'x$set(m)' initialises x to m value, with a NULL inverse
    ##      'x$get()' returns x
    ##      'x$setinv(inv)' initialises the value of 'x' inverse
    ##      'x$getinv()' returns the inverse of 'x'
    
    inv <- NULL
    set <- function(m) {
        x <<- m
        ## Above: when 'x' value is changed, inv automaticaly switched to 
        ## NULL value
        inv <<- NULL 
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## Function that computes the inverse of the special "matrix" returned 
    ## by makeCacheMatrix. If the inverse has already been calculated (and 
    ## the matrix has not changed), then the cachesolve should retrieve the
    ## inverse from the cache.
    ## Input:
    ##  - 'x' : special matrix returned by makeCacheMatrix
    ##  - (optional: parameters of 'solve' function to inverse matrix)
    ## Output:
    ##  - inverse of the matrix even:
    ##      - returned from x$getinv() if already computed
    ##      - or computed now, and cached for a future use
    inv <- x$getinv()
    if(is.null(inv)) {
        data <- x$get()
        if(nrow(data) == ncol(data)) {
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
        } else {
            message("Matrix is not square! Return NULL inverse")
            x$setinv(NULL)
        }
    } else {
        message("Getting cached data")
        inv
    }
}
