## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). 
## This assignment is to write a pair of functions that cache 
## the inverse of a matrix.

## Set the value of the Matrix
makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inversion
    setInversion <- function(inversion) inversematrix <<- inversion
    ## get the value of the inversion    
    getInversion <- function() inversematrix
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
}


## The following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

        inversematrix <- x$getInversion()
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    matx <- x$get()
    inversematrix <- solve(matx, ...)
    x$setInversion(inversematrix)
    inversematrix
}
