## Functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inv <<- inverse
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## The following function calculates the mean of the special "vector" created with the above function

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

