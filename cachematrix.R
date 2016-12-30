## Put comments here that give an overall description of what your
## functions do

## The function "makeCacheMatrix" takes a matrix and returns a
## "matrix" which is really a vector of four functions, which
## set the value of the matrix and clear the cache (set), 
##return the value of the matrix (get), set the value of the 
## inverse (setinv), and return the value of the inverse (getinv). 
## The setinv function doesn't do any actual computation - rather, 
## it takes a matrix as an argument and stores it as the inverse. 
## The purpose of the setinv function is to serve as an interface 
## for another function to compute the inverse for our "matrix" vector.

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL ## cache is empty when vector is created
        set <- function(y){ ## update the value of the matrix, and clear cache
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## return the value of the matrix
        setinv <- function(invers) inv <<- invers ##store a new value in the cache
        getinv <- function() inv ## return the cache
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## "cacheSolve" is the function that takes a matrix vector
## created by makeCacheMatrix, and returns the inverse, by returning
## the cahced inverse if one is already stored the matrix vector,
## and otherwise computing that inverse, updating the cache, and 
## returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## pull inverse from cache
        if (!is.null(inv)){ ## check if a matrix was cached
                message("Getting cached data...")
                return(inv) ##if so, return the cahed matrix
        }
        data <- x$get()
        inv <- solve(data, ...) ## otherwise, compute the inverse
        x$setinv(inv) ## update the cache
        inv ## and return that inverse
}
