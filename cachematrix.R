

## Method to create special matrix object
makeCacheMatrix <- function(m=matrix()) {
    mat <- m
    inv_mat <- NULL
    set <- function(m) {
        mat <<- m
        inv_mat <<- NULL
    }
    get <- function() {
        mat
    }
    setInverse <- function(inv) {
        inv_mat <<- inv
    }
    getInverse <- function() {
        inv_mat
    }
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Method to find the inverse of matrix and capture its cache. 

cacheSolve <- function(x, ...) {
    inv_mat <- x$getInverse()
    mat <- x$get()
    if(!is.null(inv_mat)) {
        message("getting cached data")
    } else {
        inv_mat <- solve(mat, ...)
        x$setInverse(inv_mat)
    }
    inv_mat <- makeCacheMatrix(inv_mat)
    inv_mat$setInverse(mat)
    inv_mat
}
