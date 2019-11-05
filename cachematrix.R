## Function that creates a special matrix and allows caching its inverse for later use
## through lexical scoping.
## Note: assuming here that the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
    # initializing xinv
    xinv <- NULL
    
    # set the value of the matrix
    # x and xinv are in the parent env of set, so we use <<-
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the inverse matrix
    setinv <- function(solve) xinv <<- solve
    
    # get the inverse matrix
    getinv <- function() xinv
    
    # returning a list
    list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## Function that assist makeCacheMatrix to retrieve the inverse matrix,
## if the matrix has not changed, and otherwise computes the inverse with solve().
## Note: assuming here that the matrix is invertible.

cacheSolve <- function(x, ...) {
    # getting the inverse matrix
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached inverse matrix")
        return(xinv)
    }
    
    # otherwise we compute the inverse
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}

