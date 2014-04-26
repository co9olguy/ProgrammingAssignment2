## cachematrix.R:
## Contains two functions, makeCacheMatrix and cacheSolve, which
## can be used to cache the inverse of a matrix

## makeCacheMatrix:
## function for creating and modifying a special "matrix" object, 
## which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # new cachematrices do not yet have inverse cached
        inv <- NULL
        
        # set cachematrix based on argument x 
        # (again, no inverse yet cached)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get cachematrix
        get <- function() x
        
        # set inverse of cachematrix
        setinverse <- function(inverse) inv <<- inverse
        
        # get inverse of cachematrix
        getinverse <- function() inv
        
        # if called with no argument, will print 
        # list of method functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve:
## Function to compute the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        # attempt to retrieve inverse from cache
        inv <- x$getinverse()
        
        # if inverse is cached, return it without doing anything else
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # if inverse not cached, calculate it
        data <- x$get()
        inv <- solve(data, ...)
        
        # cache calculated inverse
        x$setinverse(inv)
        
        # return inverse
        inv
}
