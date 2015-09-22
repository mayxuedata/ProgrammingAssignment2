## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. This pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that caches its inverse.
## It contains a fucntion to 
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
    #Initial value of a matrix, which is null
    i <- NULL
    
    #Store the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #Return the stored matrix
    get <- function() x
    
    #Cache the matrix
    setinverse <- function(inverse) i <<- inverse
    
    #Return the cached matrix
    getinverse <- function() i
    
    #Return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache (assuming that the matrix supplied is always 
## invertible.)
cacheSolve <- function(x, ...) {
    # Get the cached matrix
    i <- x$getinverse()
    
    #If there is a cached value for the matrix, return it
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    
    #Otherwise, get the inverted matrix and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    #Return the inversed matrix
    i
}
