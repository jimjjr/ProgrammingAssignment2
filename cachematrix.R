## These functions create and manipulate a cached object containing a matrix
## and its inverse. This object can be used instead of a matrix 
## to save time with repeat use, because it only calculates the inverse 
## the first time it is needed and returns the inverse from cache otherwise,
## thereby avoiding spending time re-computing this value.


## Creates a container object to Store a matrix and its inverse in the cache.
## Outputs a list of functions which can be used to set and retrieve the matrix
## and its inverse. Takes an invertable square matrix as the initial input.
## The inverse matrix variable is initilized to NULL.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix_inverse) inv <<- matrix_inverse
        getinverse <- function() inv
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Checks if an inverse of a matrix object created with makeCacheMatrix
## is cached in memory. Takes the makeCacheMatrix object as input.
## Returns the cached inverse if one exists. 
## Otherwise, calculates the inverse of the matrix and caches the result.
## Assumes the given matrix is invertable--no error handling included.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                ## message("Getting inverse from cache.")
                return(inv)
        }
        ## message("Calculating inverse for the first time.")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
