## The two functions below makeCacheMatrix and cacheSolve together define, and create
## a matrix object that can be cached and then have the inverse of the
## matrix calculated.



## This function defines and creates a special matrix object that can cache its inverse.  
## The inverse of the matrix is then calculated.
makeCacheMatrix <- function(x = matrix()) {
    A = NULL
    C = function(y){
        x <<- y
        A <<- NULL
    }
    get = function(E) x
    setmatrix = x
    E=solve(setmatrix)
    
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
    
}

## This function retrieves the inverse matrix calculated in the above function
## makeCacheMatrix from cache.

cacheSolve <- function(x = matrix(), ...) {
    G<-x$getmatrix()
    if(!is.null(J)){
        message("get cached data")
        return(G)
    }
    H<-x$get
    G<-solve(H, ...)
    x$setmatrix(G)
    G
        
}
