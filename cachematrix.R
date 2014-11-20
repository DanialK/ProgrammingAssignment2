makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x 
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
        result <- x$getinverse()
        if(!is.null(result)){
            message('getting cached data')
            return(result)
        }
        data <- x$get()
        result <- solve(data)
        x$setinverse(result)
        result
}
