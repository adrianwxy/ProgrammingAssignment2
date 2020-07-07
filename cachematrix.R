## For caching the inverse of a matrix: 
## It has its own advantages when the computation is focus on
## caching the inverse of a matrix instead of repeatly computing.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function()x
    setInverse<-function(inverse)inv<<-inverse
    getInverse<-function()inv
    list(set=set,get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

## According to the previous computation of function, if the
##inverse is calculated as well as the matrix has no changes,
##the result will returned, the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          mat <- x$get()
          inv <- solve(mat, ...)
          x$setInverse(inv)
          inv
}
