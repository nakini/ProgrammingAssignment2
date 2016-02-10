## The inverse of a invertible square matrix is evaluated using the following functions. First, the matrix is store in a list-object using the function "makeCacheMatrix". Then the inverse is calculated using the function "cacheSolve". The first call to the cacheSolve function evaluates the inverse, then stores the result in the same matrix object cache, and returns the result, in the end. The subsequent calls to the function "cacheSolve" for the same object will only return the inverse which is stored in the matrix-object cache.

## makeCacheMatrix --
## In this method a special object is created which will hold the data and the mean values of the data. It has couple of get/set methods to retrieve/store the data and it's mean value.
## INPUT:
#   x -- A square matrix
## OUTPU:
#   A list containing all the methods.
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL

    # Functions to store and retrieve data.
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x

    # Function to store and retrieve the inverse of the data.
    setinverse <- function(mean) invMatrix <<- mean
    getinverse <- function() invMatrix

    # A list containing all the functions of the object.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
# This method takes a special matrix object created using "makeCacheMatrix" function and evaluates the inverse of the matrix and then stores the inverse in the same object. The stored inverse matrix acts like a cache if the object is not destroyed. So, when cacheSolve function is called for a nxn matrix for the first time, the computational complexity will be O(n^3). However, the computation complexity of subsequent function calls for the same matrix object is O(1), because the value is accessed from the cache.
# INPUT:
#   x -- Invertible square matrix object.
# OUTPUT:
#   Inverse matrix.
cacheSolve <- function(x, ...) {
    # Get the cache value and check whether it is NULL value or not. If it's not a NULL, then return the inverse of the matrix and exit.
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    # Get the data.
    xMatrix <- x$get()

    # Evaluate the inverse of the square matrix obtained in the previous step.
    invMatrix <- solve(xMatrix)

    # Store the data for the future use.
    x$setinverse(invMatrix)

    ## Return a matrix that is the inverse of 'x'
    invMatrix
}
