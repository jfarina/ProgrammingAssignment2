## These functions allow you to define a matrix and calculate its inverse.
## (Must be invertable, as there is no error checking.)
## If the inverse has already been calculated, the result will be retrieved
## from the parent environment.


## This function returns a list of subfunctions for setting and getting a
## matrix and an inverse

makeCacheMatrix <- function(x = matrix()) {

    ## In the current enviroment, set "inv" to NULL
    inv <- NULL
    
    set <- function(y) {
      x   <<- y     ## Find "x" in parent environments and redefine to "y"
      inv <<- NULL  ## Since "x" is newly redefine, wipe out the old "inv"
    }
    
    get <- function() x     ## return "x"
    
    setinv <- function(inverse) {
        inv <<- inverse     ## Find "inv" in parent frames and redefine
    }
    
    getinv <- function() inv  ## return "inv"
    
    ## Set up the list of subfunctions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}







## Check the parent frames for the inverse.  If not found, calculate it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  
}
                                                                               