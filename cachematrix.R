## the following 2 functions takes a matrix and create a special object for that matrix
## that can cache its inverse matrix, and load it upon request instead of recalculating it

## makeCacheMatrix set the matirx for the inverse calcualtion and chace the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSove calculate the inverse matrix for the given matrix, but if it is already exist 
## cacheSolve will just return it form the cache and not recalculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
