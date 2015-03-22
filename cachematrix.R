## Matrix inversion might sometimes be a computational expensive and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly. Below
## are two functions that are used to create a special object that stores a numeric 
## matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a 
## list containing 4 functions: set(), get(), setinv(), getinv().

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y  # y is actually the input matrix x
        inv <<- NULL # re-initialize the inv
    }
    # "set" could be used to manually re-set the matrix
    
    get <- function() x
    
    setinv <- function(invMat) inv <<- invMat 
    # "set" could be used to manually re-set the inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)) { # inv has already been cached
        message("getting cached data")
        return(inv)
    }
    
    # if inv has not been cached
    data <- x$get()
    inv <- solve(data, ...) # compute the inverse of x
    x$setinv(inv)
    inv
}
