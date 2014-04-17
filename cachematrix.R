## makeCacheMatrix and cacheSolve together enable a faster access of matrix inverse after
## the initial calculation, through "caching" the inverse of the given matrix.

## makeCacheMatrix creates a "special" matrix and returns a list of 4 functions 
## that enable more efficient inverse access using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
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
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) #alternatively, could have used corpcor package
    x$setinverse(i)
    i
}
