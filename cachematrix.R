## Solution to Week 3, Programming Assignment 
## 21 February 2015


makeCacheMatrix <- function(x = matrix()) {

    # Return a list containing four functions - set, get, setinverse and getinverse

    m <- NULL
    
    # the set function returns the matrix, and sets the cached version to null
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # the get function returns the original matrix
    get <- function() x
    
    # the setinverse function will calculate the inverse of the matrix, and also
    # store the results in the cache
    setinverse <- function(solve) m <<- solve

    # the getinverse function returns the cached inverse matrix, or null if the
    # inverse isn't already calculated
    getinverse <- function() m
    
    # return the 
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
      
}

cacheSolve <- function(x, ...) {

    # Check whether there is a cached result for the inverse of a matrix.
    # If there is, return that cached result
    # If there isn't, calculate the invoice using "solve" and return that, and
    # also cache the result so it's available next time the function is called
    
    # Get the inverse from the cache    
    m <- x$getinverse()
    
    # if there's already a cached result, use that.
    if(!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
    
    # Otherwise, calculate the inverse using "solve"
    data <- x$get()
    m <- solve(data)
    
    # store the cached result so it's available in future
    x$setinverse(m)
    
    # return the result
    m
}