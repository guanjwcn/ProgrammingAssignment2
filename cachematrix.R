## The purpose of the function of makeCacheMatrix is to make inverse function of matrix
## The input to the function should be a matrix

## The output of this function is a list of matrices

makeCacheMatrix <- function(x = matrix()) {

  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmat <- function(solve) mat <<- solve
  getmat <- function() mat
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
  
}


## This function returns the inverse of a matrix. 
## It first checks whether the inverse is catched, if not, return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat <- x$getmat()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setmat(mat)
  mat
  
}
