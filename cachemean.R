## A modified version of the example functions from README.md

# Example
# vec <- makeVector(1:10)       # creates a list of functions vec,
#                               # assigns the vector 1:10 to vec$set in global(?) environment
# vec$set (5:15)                # assign a different vector
# cachemean(vec)                # calculates the mean using functions created in
#                               # makeVector, because non is cached
# cachemean(vec)                # retrieves cached mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("assigning data, calculating mean, setting mean, returning mean")
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}