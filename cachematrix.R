## Program for Caching the Inverse of a Matrix 

## Function that creates a special "Matrix" object that can cache its Inverse 

makeCacheMatrix <- function(m = matrix()) {
    
    ## Initializing the Inverse property
    x <- NULL
    
    ## Method for Setting the Matrix object 
    set <- function(matrix) {
        m <<- matrix
        x <<- NULL
    }
    
    ## Method to get the matrix object 
    get <- function() {
        m
    }
    
    ## Method to set the Inverse of a Matrix 
    setInverse <- function(inverse) {
        x <<- inverse
    }
    
    ## Method to get the Inverse of a Matrix 
    getInverse <- function() {
        x
    }
    
    ## Return a list of the methods used 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve function below computes the inverse of the special "Matrix" returned 
## by the makeCacheMatrix function above. If the inverse is already calculated 
## i.e. the matrix is unchanged then cacheSolve function will retrieve the
## inverse from the cache 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return the Inverse of the matrix if it is already set 
    if(!is.null(m)) {
        message("Getting Cached Data")
        return(m)
    }
    
    ## Get the Matrix 
    data <- x$get()
    
    ## Inverse calculation using Matrix Multiplication
    m <- solve(data) %*% data
    
    ## Set Inverse to object 
    x$setInverse(m)
    
    ## Return Matrix 
    m
}
