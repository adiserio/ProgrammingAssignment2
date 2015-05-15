## Author:   Angela Di Serio
## date: May-2015

## Function: makeCacheMatrix
## Creates a "special matrix" that consists of a list of function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function (inv) m <<- inv
    getinverse <- function () m
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## Function: cacheSolve
##
## Calculates the inverse of a "special matrix".
## First, it checks whether the inverse has already been calculated.
## If so, it gets the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache
## 1. set the value of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
