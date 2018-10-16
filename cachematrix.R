## Coursera R Programming Week 3 Programming Assignment #2
## These functions cache the inverse of a matrix
## 
## This function produces a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        
##  This function set the matrix value
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
##  This function gets the matrix value        
        
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function returns the matrix that is the inverse of 'x' , the makeCacheMatrix
## If the inverse has already been calculated, and the 
## matrix has not changed , the cached inverse is returned. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
       
