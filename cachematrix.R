## Computing a Matrix takes a lot of time
## Below Functions will calculate inverse of a Matrix
## They will store the result as cache if the input matrix is same

## This function creates a vector and return below in a list
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(iMatrix) inverseMatrix <<- iMatrix
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## The following function calculates the inverse of matrix created with the above function
## It will also check if result is already in cache and skips the computation and return the result 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getInverse()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  matrix <- x$get()
  iMatrix <- solve(matrix, ...)
  x$setInverse(iMatrix)
  iMatrix
}
