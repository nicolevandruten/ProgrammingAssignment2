## Nicole van Druten
## Programming Assignment 2

## The following two functions cache the inverse of a matrix. If the inverse is 
## already set, the function will return the inverse and if it has not yet been 
## calculated, it will be calculated and then cached.


## The following function outputs a list containing functions to get the 
## the matrix, get the inverse of the matrix, set the matrix and set the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse.matrix <- function(inverse.matrix) m <<- inverse.matrix
        getinverse.matrix <- function() m
        list(set = set, get = get,
             setinverse.matrix = setinverse.matrix, 
             getinverse.matrix = getinverse.matrix)
}


## The following function first checks if the inverse has already been set and 
## if it has, it gets the inverse from the cache and returns it. However if the 
## inverse matrix has not been set, the cacheSolve function takes the matrix 
## created with the makeCacheMatrix function and solves for the inverse and 
## then caches this inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse.matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse.matrix(m)
        m
}