## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create a list of 4 functions which set, get the matrix 
## and set, get the inverse of the matrix.
## when matrix doesn't change inverse is not set to NULL

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        if( !identical(x, y)){
            x <<- y
            ## invalidation of the cache when matrix changes.
            inv <<- NULL
        }
    }
    getmatrix <- function() x
    setinv <- function(Xinv) inv <<- Xinv
    getinv <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
        setinv = setinv, getinv = getinv)
}

## solves for the inverse of the matrix, fetches from cache if present.

Cache <- makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Cache$setmatrix(x)
        inverse <- Cache$getinv()
        if(is.null(inverse)){
            inverse <- solve(x, ...)
            Cache$setinv(inverse)
            return(inverse)
        }
        else{
            message("Fetching from Cache")
            return(inverse)
        }
}
