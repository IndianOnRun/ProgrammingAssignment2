## Functions to cache the inverse of a matrix in the environment, if not done already.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize 'inverse' property
        i <- NULL
        
        ## Set the matrix
        set <- function(mat) {
                x <<- mat
                i <<- NULL
        }
        
        ## Get the matrix
        get <- function() {
                ## Return the matrix
                x
        }
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Get the inverse of the matrix
        getInverse <- function() {
                ## Return the 'inverse' property
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## Return the inverse from cache if its already set
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
        }
        
        ## Get the matrix from object
        data <- x$get()
        
        ## Inverse of Matrix
        i <- solve(data)
        
        ## Set the inverse to the object
        x$setInverse(i)
        
        ## Return a inverse matrix
        i
}
