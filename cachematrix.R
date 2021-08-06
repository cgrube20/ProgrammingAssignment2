## For makeCacheMatrix
## inv represents null objects, spec allows me to specify the value of the matrix,
## The first two <<- functions allow the variables to change in the parent levels
## pull gets the value of the matrix
## specInverse specifies the value of inverse, <<- is needed again
## pullInverse gets the value out
##
## For cacheSolve
## inv is used for to pull the inverse
## if leads to the return function is criteria are met
## mat pulls the matrix
##I tested using a 2 by 2 pmatrix function

## makeCacheMatrix will create and store our initial matrix values for later calculation


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        spec <- function(y){
                x <<- y
                inv <<- NULL
        }
        pull <- function() {x}
        specInverse <- function(inverse) {inv <<- inverse}
        pullInverse <- function() {inv}
        list(spec = spec, pull = pull, specInverse = specInverse, pullInverse = pullInverse)
}


## This function will return the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$pullInverse()
        if(!is.null(inv)){
                message("pulling cache data")
                return(inv)
        }
        mat <- x$pull()
        inv <- solve(mat, ...)
        x$specInverse(inv)
        inv
}
