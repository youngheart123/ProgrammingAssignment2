## Put comments here that give an overall description of what your
## functions do

## Description--
##   This function creates an object of class "list" of four functions 
##   that can cache the inverse of a gvien matrix
## Usage--
##   makeCacheMatrix(x)
## Arguments--
##   x-- an object of class "matrix" of equal row number and column number
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
          if (nrow(x) != ncol(x)) {
            message("input matrix is not square")
            NULL
          } else {
            x
          }
        }

        setInv <- function(invm) m <<- invm
        getInv <- function() m
        list(set = set, get = get,
             setinv = setInv,
             getinv = getInv)
}


## Description--
##   Return a matrix that is the inverse of the matrix that is created by
##   makeCacheMatrix() and that may be obtained by by x$get(). 
##   If the inverse has been calculated, use the cached inverse and skip calculation. 
##   If not, calcuate the inverse and save it in the cache.
## Usage--
##   cacheSolve(x)
## Argument--
##   x-- an object of class "list" that is created by makeCacheMatrix()
cacheSolve <- function(x, ...) { 
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
		
}
