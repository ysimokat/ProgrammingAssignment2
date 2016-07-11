## @ysimokat Programming Assignment 2
## R program is functions used to cache matrix inverse
## matrix must be invertible

## First function makeCacheMatrix
## Creates special "matrix" object that is a cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        # Inverse starts as null
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        # Last line is what function returns
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Second function cacheSolve
## Accepts as input output of the first function
## Figures out whether there is a cached value already or not and gets or sets it based on if it is there.
cacheSolve <- function(x, ...) {
        
        # Get the inverse
        inv <- x$getinverse()
        # Check if emptpy and if not return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Calc inverse, cache, and return.
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}