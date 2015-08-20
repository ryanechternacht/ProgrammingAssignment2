## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'x'
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
