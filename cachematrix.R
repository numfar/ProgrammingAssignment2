## File contains two functions, 'makeCacheMatrix' and 'cacheSolve'
## 'makeCacheMatrix' creates and maintains the cache of the inverse for the matrix
## and 'cacheSolve' uses 'makeCacheMatrix' to get and set the inverse of the matrix.

## Makes cache of inverse of matrix
## Assumes x is and invertible martrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(iny){
        i <<- iny
    }
    getInv <- function(){
        i
    }
    list(get=get,set=set,getInv=getInv,setInv=setInv)
}


## Callculates and returns the inverse of a matrix if not in cache,
## else returns the cached matrix inverse.
## Assumes x is an invertible matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    dInv <- x$getInv()
    if(!is.null(dInv)){
        message("Getting cache data.")
        return(x$getInv)
    }
    d <- x$get()
    dInv <- solve(d,...)
    x$setInv(dInv)
    dInv
}
