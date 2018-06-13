## Caches a Matrix Inverse

## Matrix inversion is computationally intensive. 
## This function creates a matrix option that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minv <<- inverse
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves for the inverse of a matrix
## If the inverse has already been calcualted it retrieves it from the cache rather than recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)){
      return(minv)
    }
    matrix <- x$get()
    minv <- solve(matrix,...)
    x$setinverse(minv)
    minv
}
