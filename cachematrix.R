# In order to minimize calculations and maximize speed, these functions
# create a cache for solving the inverse of a matrix so that the calculation
# is only carried out if it has not been already. 

# Creates a cache ("inverse_cache") for the inverse operation and returns a 
# special matrix object comprised of functions to access/modify the cache

makeCacheMatrix <- function(x = matrix()) {

    inverse_cache <- NULL #initialize the cache to NULL
    
    set <- function(y){
        x <<- y
        inverse_cache <<- NULL
    }
    
    get <- function() x
    setinverse <- function(local_inverse) inverse_cache <<- local_inverse
    getinverse <- function() inverse_cache
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Receives a cache (empty or not) input. If cache is empty, 
# cacheSolve calculates the inverse and stores the inverse into the cache. 
# Otherwise, the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    d = x$get()
    inv = solve(d)
    x$setinverse(inv)
    inv
    
        
}
