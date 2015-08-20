## Put comments here that give an overall description of what your
## functions do

## This function creates a wrapper around a matrix that allows the ability to 
## retrieve and change the internal matrix. Additionally 2 methods are provided for 
## retrieving and setting the inverse of the internally held matrix. Finally, if the 
## wrapper is instructed to change which matrix it holds, it will unset its cached inverse

makeCacheMatrix <- function(A = matrix()) {
    inv <- NULL
    set <- function(B) { 
        A <<- B
        inv <<- NULL
    }
    get <- function() { A }
    setInverse <- function(inverse) { inv <<- inverse }
    getInverse <- function() { inv }
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This method returns the inverse of the matrix held by the special wrapper created by
## makeCacheMatrix(). If the wrapper has never calculated the inverse of its matrix, it will 
## do so and then cache that result. If it has already calculated the inverse, it will return
## the cached value (thus, saving unnecessary computation). 

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'A'
    inv <- A$getInverse()
    if(!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
    
    data <- A$get()
    inv <- solve(data)
    A$setInverse(inv)
    inv
}
