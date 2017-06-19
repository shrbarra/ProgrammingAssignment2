## Receives the "special" vector created in makeCacheMatrix and
## returns the inverse of matrix X. If the inverse has already
## been calculated, then it returns the message "Getting
## cached data" and the cached value

## makeCacheMatrix creates a vector of functions
## which will be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     setInv <- function(inverse) inv <<- inverse
     getInv <- function() inv
     
     list(inv=inv, get=get, setInv=setInv,
          getInv=getInv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
     inv <- x$getInv()
     if(!is.null(inv)) {
          message("Getting cached data...")
          return(inv)
     }
     
     data <- x$get()
     inv <- solve(data)
     x$setInv(inv)
     
     inv
}
