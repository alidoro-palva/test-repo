makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setmat <- function(mymat) {
        x <<- mymat
        inv <<- NULL
    }
    
    getmat <- function() x
    
    setinv <- function(myinv) inv <<- myinv
    
    getinv <- function() inv
    
    list(setmat = setmat,
         getmat = getmat,
         setinv = setinv,
         getinv = getinv
    )
}

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        ## message("getting cached data")
        return(s)
    }
    data <- x$getmat()
    s <- solve(data)
    x$setinv(s)
    s
}