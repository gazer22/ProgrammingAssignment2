## Creates functions which cache and return the inverse of a matrix
## usage:
##  x <- matrix(c(3,2,6,2), 2, 2)
##  x_aug <- makeCacheMatrix(x)
##  cacheSolve(x_aug)
##  x_aug$getinv()

## create a matrix which can store its inverse:
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # use set to initialize or change the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # use get to return the current matrix
    get <- function() x
    
    # use setinv to set the inverse
    setinv <- function(inv) inverse <<- inv
    
    # use getinv to get the inverse (cacheSolve must be run first for it to 
    #   return a non-NULL answer)
    getinv <- function() {
         inverse   
    }
    
    # this has to do with packaging
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## calculate inverse of a matrix, simply return cached version if 
## already calculated and the matrix hasn't changed
## Note: assumes x is a square, invertible matrix

cacheSolve <- function(x, ...) {
    
    # pull inverse from current matrix structure
    inverse <- x$getinv()
    
    # if the inverese exists, return it
    if(!is.null(inverse)) {
        #message("getting cached data")  #debugging
        return(inverse)
    }
    
    # you only get here if inverse is not yet calculated
    # calculate inverse and return it
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    inverse
}
