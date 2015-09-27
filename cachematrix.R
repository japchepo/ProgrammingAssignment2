## These functions create the inverse of a matrix by using the solve R function.  
## The makeCacheMatrix function creates a special matrix that can cache its inverse.
## The cacheSolve function checks if the inverse has been cached and retrves that value
## or does the inverse and stores it in cache.

## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x # returns matrix x
    }
    setInverse <- function(inverse){
        inv <<- inverse
    }  
    getInverse <- function() {
        inv # get the cached inverse
    }
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}# End makeCacheMatrix 


## This function computes the inverse of the special “matrix” 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setInverse(inv)
    inv # return inv
}
