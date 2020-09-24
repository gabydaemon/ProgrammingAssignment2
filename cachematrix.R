makeCacheMatrix <- function(ogMatrix = matrix()) {
        matrixInverse <- NULL
        
        set <- function(funcMatrix){
                ogMatrix <<- funcMatrix
                matrixInverse<<- NULL
        }
        
        get <- function(){
                ogMatrix
        }
        
        setInv <- function(inv){
                matrixInverse <<- inv
        }
        
        getInv <- function(){
                matrixInverse
        }
        list( set = set, 
              get = get, 
              setInverse = setInv, 
              getInverse = getInv)
}


cacheSolve <- function(func, ...) {
        ## check if inverse has already been set
        calcInverse <- func$getInv()
        ## is it empty? if not, run this block
        if(!is.null(calcInverse)){
                message("getting cached inverse")
                return(calcInverse)
        }
        
        ## get the base matrix to inverse
        baseMatrix <- func$get()
        ## inverse nbase matrix and put that shit 
        ## into a new variable
        calcInverse <- solve(baseMatrix, ...)
        
        func$setInv(calcInverse)
        
        calcInverse
}

##this took me so long to write!!!!!
