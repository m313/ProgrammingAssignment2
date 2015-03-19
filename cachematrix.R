## The functions makeCacheMatrix and cacheSolve are used to calculate and 
## automatically cache the inverse of a matrix. This way, recalculating the same
## inverese can be avoided in order to save computing time.
## makeCacheMatrix is used to create a special "matrix object" with functions to
## set and get the matrix and its inverse. cacheSolve takes a special "matrix
## objects" and calculates its inverse or looks it up from cache if possible.
## makeCacheMatrix uses the '<<-' operator to store the matrix and its inverse
## in the parent environment of the four functions so that all of the four 
## functions can access them.


# ## Example 
# specialMatrix <- makeCacheMatrix()       # create a special matrix "container"
# mat <- matrix(c(2,1,5,3),nrow=2,ncol=2)  # define a matrix
# specialMatrix$set(mat)      # set the value of the matrix
# specialMatrix$get()         # get the value of the matrix
# cacheSolve(specialMatrix)   # calculate the inverse matrix (and cache it)
# cacheSolve(specialMatrix)   # retrieve the inverse matrix from cache


## makeCacheMatrix takes a marix as an input and creates a special "matrix", 
## which is really a list of 4 functions to
##   1. set the value of a matrix in the parent environment
##   2. retrieve the value of a matrix
##   3. set the inverse in the parent envorionment
##   4. retrieve the inverse of a matrix
## These functions are used as helpers for cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes a special "matrix" as an input and returns its inverse. It
## calls makeCacheMatrix$getinv() to look for a cached inverse. If no cached 
## inverse is available, it calculates the inverse and also caches the result of 
## the calculation

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    message("calculating inverse")
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(mat)
    inv
}
