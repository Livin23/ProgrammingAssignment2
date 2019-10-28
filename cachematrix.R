

## Method to create special matrix object

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function( matrix ) {
            mat <<- matrix
            inv <<- NULL
    }

  get <- function() {
    	
    	mat
    }

  setInverse <- function(inverse) {
        inv <<- inverse
    }
  getInverse <- function() {
        
        inv
    }

list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Method to find the inverse of matrix and capture its cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()
if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
 }
data <- x$get()

 mat <- solve(data) %*% data
  x$setInverse(mat)
mat
   
}
