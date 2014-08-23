## These functions create a "special" matrix which caches it's inverse, 
## calculated by the second the second function. 
## This is similar to properties and methods in object orientated programming.

## This function returns a list, containing the original matrix, and 
## its inverse (once calculated)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get, setinverse = setinverse,  getinverse = getinverse)
    
}


## This function calculates and stores the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data)
    
    x$setinverse(m)
    
    m
        
}
