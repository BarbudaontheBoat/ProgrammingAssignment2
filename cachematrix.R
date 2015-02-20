## This function creates a list containing the functions:
##          set <- set the value of the matrix
##          get <- get the value of the matrix
##          setinverse <- set the value of the matrix inverse
##          getinverse <- get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix x
## If x is a singular matrix, a warning message will apear
cacheSolve <- function(x, ...) {
    
    if(det(x$get()) == 0) {
        
        message("The input matrix is singular, has no inverse")
    }
    if(det(x$get()) != 0) {
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
            
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }
}
