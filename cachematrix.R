
## This function creates a Cache Matrix, which functions like 
## set -> To set the value of Matrix and set the Inverse as NULL
## get -> Returns the Matrix Value
## setInverse -> To set the matrix Inverse value
## getInverse -> To get the matrix inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializes the matrixInverse to NULL
  matrixInverse <- NULL 
  
  ## Method to set the input Matrix and matrixInverse to NULL 
  set <- function(y)
  {
	## Note not local scope
    x <<- y
    matrixInverse <<- NULL
  }
  
  ## to get the input Data
  get <- function()
  {
    x
  }
  
  ## setInverse of matrixInverse
  setInverse <- function(inverse)
  {
    matrixInverse <<- inverse
  }
  
  ## to get the Inverse of the Matrix
  getInverse <- function()
  {
    matrixInverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve Function uses the solve function to derive the matrix and set that to the Matrix Inverse

cacheSolve <- function(x, ...) {
  tempMatrixInverse <- x$getInverse()
  if(!is.null(tempMatrixInverse)) {
    message("getting cached data")
    return(tempMatrixInverse)
  }
  matrixData <- x$get()
  tempMatrixInverse <- solve(matrixData, ...)
  x$setInverse(tempMatrixInverse)
  tempMatrixInverse
  ## Return a matrix that is the inverse of 'x'
}