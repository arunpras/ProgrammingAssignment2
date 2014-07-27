## This script accepts a matrix and loads it to cache and returns
## the inverse matrix

##------- Usage---------##
#  source("cachematrix.R")
#  a<-matrix(rnorm(16),4,4)
#  b<-makeCacheMatrix()
#  b$set(a)
#  cacheSolve(b)
#  cacheSolve(b) %*% a

## makeCacheMatrix  
##    This function accepts an input matrix creates a list  
##    containing functions
##      set() loads a matrix 'x' in cache.
##      get() returns the values of matrix 'x'.
##      getinv() returns the values of inverse matrix 'm'.
##      setinv() loads the inverse matrix 'm' in cache

makeCacheMatrix <- function(x = matrix()) {
        ## The inverse matrix in cache is initialized to NULL
        ## to prevent preemptive calling of getinv() 
        ## before setinv() 
        m <- NULL
        
        ## This sets new value to matrix 'x' 
        ## and for each new input matrix resets inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## This returns the values of the input matrix 'x'
        get <- function() x
        
        ## This sets the values to the inverse matrix 'm'
        setinv <- function(inv) m <<- inv
        
        ## This returns the values of the inverse matrix 'm'
        getinv <- function() m
        
        ## Return values of the function is a list of the four 
        ## above functions. These four functions can be 
        ## accessed with $ operator
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve  
##    This function accepts an input matrix and  
##    calculates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        
        ## if inverse matrix 'm' is already set
        ## get values directly from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## get the values of the input matrix
        data <- x$get()
        
        ## calculate the inverse matrix for the input matrix
        m <- solve(data, ...)
        
        ## write the inverse matrix values into cache
        x$setinv(m)
        
        ## return the inverse matrix
        m
}
