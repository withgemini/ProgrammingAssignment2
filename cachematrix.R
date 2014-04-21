## makeCacheMatrix and cacheSolve together enable a faster access of matrix inverse after
## the initial calculation, through "caching" the inverse of the given matrix.
## Both functions are largely based on the makeVector() and cachemean() functions, provided in the
## Programming Assignment 2 instructions

## makeCacheMatrix creates a "special" matrix and returns a list of 4 functions 
## that enable more efficient inverse access using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## default inverse value 
    set <- function(y) {
        x <<- y
        i <<- NULL ## default inverse value 
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse 
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## cacheSolve calculates inverse for a "special" matrix, created using makeCacheMatrix. 
## This function stores the inverse and returns it, when the function is called on the matrix 
## for the first time. When the function is called on the "special" matrix,for which the inverse 
## has been already calculated, it accesses its stored value without any recalculations. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    ## if the inverse has been calculated already, access it
    if(!is.null(i)) { 
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) ## alternatively, could have used corpcor package for inverse calculation
    x$setinverse(i)
    i
}
