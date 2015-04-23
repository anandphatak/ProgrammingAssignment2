## Following functions help save computation time by storing inverse of a 
## matrix in the cache, so that it is not computed each and every time even
## though the underlyign matrix has not changed. This will save processing 
## time and resources.



## makeCacheMatrix: This function acts as a cache that stores the matrix, and 
## its inverse (when computed inverse of matrix is supplied to it)
##
## List of functions: 
##    setmatrix(m):        This takes a matrix argument and stores it in the 
##                         cache. It also sets any inverse of matrix previously 
##                         stored in the cache
##    getmatrix() :        Returns matrix stored in the cache
##    setmatrixinverse(m): Sets inverse of matrix in the cache
##    getmatrixinverse():  Returns inverse of matrix stored in the cache
makeCacheMatrix <- function(x = matrix()) {
    # This is the cached inverse
    inverseOfMatrix <- NULL
  
    # Set a matrix and invalidate any previously stored inverse
    setmatrix <- function(m) {
        x <<- m
        inverseOfMatrix <<- NULL
    }
  
    getmatrix <- function() x
  
    setmatrixinverse <- function(inv) inverseOfMatrix <<- inv
  
    getmatrixinverse <- function() inverseOfMatrix
  
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix, 
         setmatrixinverse = setmatrixinverse, 
         getmatrixinverse = getmatrixinverse)

}


## cacheSolve: This function takes an argument which is a 
##             function "makeCacheMatrix". It first checks if there is any 
##             inverse of matrix returned by makeCacheMatrix. If no inverse is
##             found in the cache, it computes the inverse, stores it in the 
##             cache for future use, and returns it
cacheSolve <- function(x, ...) {
    # Obtain inverse from makeCacheMatrix function
    inverseOfMatrix <- x$getmatrixinverse()
  
    # Check if the value returned is not null. If not null, we will return 
    # cached value
    if(!is.null(inverseOfMatrix)) {
        message("Returning inverse of matrix from cache")
        return(inverseOfMatrix)
    }
  
    # Nothing was found in the cache. So compute the inverse, set it in the 
    # cache and then return value
    inverseOfMatrix <- solve(x$getmatrix())
    x$setmatrixinverse(inverseOfMatrix)
  
    inverseOfMatrix
}
