## This function creates a special "matrix" object that can cache its inverse.
## Matrix x is assumed to always be inversable.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set=set,get=get,
         setinverse=setinverse,getinverse=getinverse)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.  If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache stored in the
## makeCacheMatrix "singleton" object.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
