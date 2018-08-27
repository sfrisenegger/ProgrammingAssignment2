## History Log
## Createdby: Sue Liao
## Date:      2018-08-26
## Description:
##
##    Matrix inversion is usually a costly computation and the following functions are
##    used to create a special object that stores a matrix and caches its inverse.



## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL               			     ##sets the NULL as a placeholder for a future value
        
        set <- function(y) {      			     ##defines a function to set the matric x, to a new matrix, y,
                x <<- y                                      ##and resets the inverse, inv, to NULL
                inv <<- NULL
        }

 
        get <- function() x        			     ##defines a function to get the matric x passed via the parent function

        setInverse <- function(inverse) inv <<- inverse      ##sets the value inverse to inv

        getInverse <- function() inv                         ##returns the value of inv  

        list(set = set,                                      ##returns the special object containing all the functtions defined above
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function (cacheSolve) computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {


        inv <- x$getInverse()                              ## Returns a matrix that is the inverse of 'x'

        if (!is.null(inv)) {                               ## checks if the inverse exists in cache, if so, returns the cache value
                message("getting cached data")
                return(inv)
        }

        mat <- x$get()                                    ## gets the values using get function and assign it to matrix, mat     

        inv <- solve(mat, ...)                            ## calculates inverse of matrix 

        x$setInverse(inv)                                 ## sets inverse of matrix,inv 

        inv                                               ## returns values of inverse of matrix, inv 
}