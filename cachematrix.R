## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## call this first to get the special matrix object which is passed to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    # this is the result that is cached. Init to NULL. 'i' for inverse
    i <- NULL
    # call $set to assign a new matrix
    set <- function(y) {
        x <<- y            # just copying the matrix to the super environment
        i <<- NULL         # inverse hasn't been calculated yet
    }
    get <- function() x    # $get will return the matrix
    setinverse <- function(inv) i <<- inv  # this caches the inverse
    getinverse <- function() i             # get the cached inverse
    list(set = set, get = get,             # returns a list of functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()    # will either get NULL or a previously calculated inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()       # inverse hasn't been calculated, get matrix
    i <- solve(data, ...) # use solve to calculate inverse
    x$setinverse(i)       # cache the inverse
    i                     # returns inverse
}
