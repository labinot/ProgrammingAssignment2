## The below functions provide a way to cache an invers of a matrix.

## This function accepts a matrix and creates a special "Matrix" with the ability 
## to cache it's inverse.
## It returns a list, with functions that can be accessed to manipulate the original
## Matrix passed to it.
makeCacheMatrix <- function(x = matrix()) {
        ##initialize the solved matrix to Null
        m <- NULL
        ## function that sets matrix data 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## function that gets matrix data
        get <- function() x
        
        ##a function to set the solved inverse matrix that can be accessed
        ##from outside functions envirnomet
        setSolve <- function(solve) m <<- solve
        
        ##a function to get the solved inverse matrix
        getSolve <- function() m
        
        ## calls all of the functions above and returns the list
        ## with all the functions to get, set, setSolve and getSolve
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)        
}

## This function solves the inverse of the matrix created with the function makeCacheMatrix. 
## First checks to see if the inverse has already been calculated. If it has, it gets the solved 
## matrix from cache and skips the computation. Otherwise, it calculates the inverse matrix and 
## sets solved matrix in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        ##if data are cached return the solved matrix and exit the function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##get matrix data
        data <- x$get()
        ##solve the inverse of the matrix
        m <- solve(data, ...)
        ##cache the data for later use
        x$setSolve(m)
        ##return the inverse matrix
        m
}