## This file contains two functions that allow to speed up
## the computation of the inverse of a matrix through the
## use of cache 
## Example usage:
## c=rbind(c(1, -1/4), c(-1/4, 1)) 
##> m <- makeCacheMatrix(c)
##> cacheSolve(m)
##          [,1]      [,2]
##  [1,] 1.0666667 0.2666667
##  [2,] 0.2666667 1.0666667
## > cacheSolve(m)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## creates a special matrix object containing a list of functions
## to get and set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}



## Returns a matrix that is the inverse of 'x' using the cache 
## (inverse is not computed if the result is already stored
## in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv


}
