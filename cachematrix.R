## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        
        ## object methods below that can be called upon by other functions
        
        set <- function(y) {
                x <<- y
                s <<- NULL      ## s is the result of the inversion
                                ## and is reset to NULL every time
                                ## makeCacheMatrix is called
        }
        
        get <- function() x

        setsolve <- function(solve) s <<- solve
        getsolve <- function() s

        ## create a list of the four functions (ie object methods) above
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)       ## if s is already cached, show it...
        }
        data <- x$get()         ## otherwise, grab the matrix and...
        s <- solve(data, ...)   
        x$setsolve(s)           ## invert it
        s
}
