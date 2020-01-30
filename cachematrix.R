## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    mat_inverse <<- NULL
  }
  get <- function() {
    x
  }
  setMatrixInverse <- function(mi) {
    mat_inverse <<- mi
  }
  getMatrixInverse <- function() {
    mat_inverse
  }
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
  mat_inverse <- x$getMatrixInverse()
  if (!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  matrix <- x$get()
  mat_inverse <- solve(matrix, ...)
  x$setMatrixInverse(mat_inverse)
  mat_inverse
}
