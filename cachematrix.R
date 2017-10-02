## These two functions can be used to create a special object that stores a matrix and caches its inverse.

## This function creates a list that contains a function that set the values of a matrix, get the values, set the inverse of
## the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns a matrix that is the inverse of a special matrix created with the makeCacheMatrix function. If the 
## inverse of the matrix has already been calculated, it returns a message ("getting cached data") and retrive the inverse
## from the cache, otherwise it caluclates the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
