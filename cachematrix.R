
## This function creates a special matrix object that can cache ist inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse<-NULL
    
    setMatrix<-function(y){
        x<<-y
        inverse<<-NULL
    }
    
    getMatrix<-function() x

    
    setInverse<-function (inv){
        inverse<<-inv
    }
    
    getInverse<-function()inverse
    
    list(setMatrix=setMatrix,getMatrix= getMatrix,setInverse=setInverse, getInverse= getInverse)
}


##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inverseCached<-x$getInverse()
    
    if(!is.null(inverseCached)){
        message("getting cached data")
        return(inverseCached)
    }
    
    matrixCached<-x$getMatrix()
    
    inverseComputed<-solve(matrixCached)
    
    x$setInverse(inverseComputed)
    
    inverseComputed
        
}
