## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL                               ## create empty variable
        set <- function(y) {                    ## creates a function to set new matrix
                x <<- y                         ## not used in this assignment
                m <<- NULL
        }
        get <- function() x                     ## creates a function to call matrix input
        setinv <- function(inv) m <<- inv       ## creates a function to save cached inverse matrix
        getinv <- function() m                  ## creates a function to call inverse matrix
        list(set = set, get = get,              ## list of functions
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

## Computes inverse of the matrix from makeCacheMatrix function above. 
## If the matrix has not changed and inverse has already been calculated, 
## then function retrieves cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()                         ## calls the getinv function from previously created "matrix"
        if(!is.null(m)) {                       ## checks for cached inverse
                message("getting cached data")  ## outputs note for retrieving cached matrix
                return(m)                       ## returns cached matrix
        }
        data <- x$get()                         ## if getinv() is null, matrix is called
        m <- solve(data, ...)                   ## inverse of matrix is calculated
        x$setinv(m)                             ## inverse is saved into special "matrix"
        m                                       ## inverse is output
        
} 
