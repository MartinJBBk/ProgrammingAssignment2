## makeCacheMatrix and cacheSolve are intended to calculate and cache 
## a matrix and its inverse

## makeCacheMatrix creates a list containing four functions:
##     set - inserts the matrix to be inverted into x and sets inv to NULL
##     get - returns x
##     setInv - sets inv to the calculated inverse of x
##     getInv - gets inv

makeCacheMatrix <- function(x = numeric()) {
    ## sets up the list that stores matrix x and its inverse

    inv <- NULL
    # if called, populates the matrix and resets the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## setup the list returned by the function
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve returns the inverse of the matrix in the list created by 
## makeCacheMatrix 
##
##     if the inverse has already been calculated and cached (i.e. getInv
##     does not return null), it returns the value retrieved by getInv
##
##     if the inverse has not been cached, it gets the data from the list,
##     calculates the inverse, stores it in inv and returns the inverse

cacheSolve <- function(x, ...) {
    # calculates or returns the stored inverse of matrix
    
    iCalc <- x$getInv()
    # if the inverse is cached returns the cached value
    if(!is.null(iCalc)) {
        message("getting cached data")
        return(iCalc)
    }

    # otherwise, calculates, stores and returs inverse
    data <- x$get()
    iCalc <- solve(data, ...)
    x$setInv(iCalc)
    iCalc
}
