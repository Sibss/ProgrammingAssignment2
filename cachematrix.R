## This function creates a special "matrix" object that can cache its inverse.

## Computing the inverse of a square matrix is done with the solve function 
## in R. If X is a square invertible matrix, then solve(X) returns its inverse. 
## Here, it's assumed that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) x_inverse <<- solve
        getinverse <- function() x_inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getinverse()
        
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        
        x_inverse
}

