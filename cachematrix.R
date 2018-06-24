## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly 
## The following functions cache the inverse of a matrix.

## This function takes a square matrix as an argument. It creates an object with 4 methods:
## set - saves matrix in the object, get - returns matrix, setinverse - sets a value of 
## inverse of the matrix, getinverse - returns inverse of the matrix. The object of this 
## kind stores matrix and its inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinverse <-function(r) inverse<<-r
        getinverse <-function() inverse
        
        list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
        
}


## This function takes object makeCacheMatrix (matrix with special methods) as an argument
## It computes and returns the inverse of the matrix if it hasn't been computed yet and 
## stores it in the cache. If inverse has been computed before for this matrix - the 
## inverse will be fetched from the cache and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        orig_matrix<-x$get()
        inverse<-solve(orig_matrix,...)
        x$setinverse(inverse)
        inverse
}
