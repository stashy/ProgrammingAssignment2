## Purpose of these functions is to limit repeated matrix "solve" calculations
## by cacheing inverse the first time it is calculated.
### 
### Example of the two Functions in use: 
###         mat <- matrix(c(1,2,3,4),nrow=2);
###         csfood <- makeCacheMatrix(mat); 
###         cacheSolve(csfood)

## Initialize functions: makeCacheMatrix creates a list of four functions
## get,set, getinverse and setinverse


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

## CacheSolve takes x, the output of MakeCacheMatrix 
## CacheSolve then executes one or two of funcitons in 
## the list x depending on whether the 
## inverse has been cached or not

cacheSolve <- function(x, ...) {
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
