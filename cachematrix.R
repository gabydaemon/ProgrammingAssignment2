## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       ##placeholder var
        inv <- NULL
       ## varName$set will set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
       ## return x
        get <- function() x
       ## inverse matrix
        setInverse <- function(inverse) inv <<- inverse
       ## return matrix inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       ##store inverse previously computed into inv
        inv <- x$getInverse()
       ## if inv isn't empty
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
       ## store previously computed matrix
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
       
       
        inv
}
