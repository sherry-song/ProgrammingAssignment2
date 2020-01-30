## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  ## initialize matrix inverse variable
  mat_inverse <- NULL
  ## define set function for saving matrix data
  set <- function(matrix) {
    ## assign input matrix data into x
    x <<- matrix
    ## set inverse to NULL
    mat_inverse <<- NULL
  }
  ## define get function for getting current matrix data
  get <- function() {
    x
  }
  ## define set function for storing matrix data
  setMatrixInverse <- function(mi) {
    mat_inverse <<- mi
  }
  ## define get function for getting inverse
  getMatrixInverse <- function() {
    mat_inverse
  }
  ## return list of four functions
  list(
    set = set,
    get = get,
    setMatrixInverse = setMatrixInverse,
    getMatrixInverse = getMatrixInverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First get the cached inverse
  mat_inverse <- x$getMatrixInverse()
  ## Check whether cached inverse exists
  if (!is.null(mat_inverse)) {
    ## If cached inverse exists, directly return it
    message("getting cached data")
    return(mat_inverse)
  }
  ## Get matrix data
  matrix <- x$get()
  ## Calculate matrix inverse by solve function
  mat_inverse <- solve(matrix, ...)
  ## Save inverse matrix to cache
  x$setMatrixInverse(mat_inverse)
  ## return inverse matrix
  mat_inverse
}
