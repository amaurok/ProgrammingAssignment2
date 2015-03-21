## These couple of functions cache the inverse of a matrix.
## The makeCacheMatrix create a special vector, containing the information
## for the matrix under processing, and the state of the prior processed matrix.
## The function cacheSolve verifies if there is a matrix inverse calculated, and
## if the matrix has not changed, in order to pull the inverse from the cache.


## Create an special matrix, to cache the state and the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
    matrixstate <<- y
  }
  getmatrix <- function() x
  
  ## Functions to get and set the inverse.
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ##Determine if the matrix has changed.
  isIdentical <- function() {
    identical(getstate(),getmatrix())
  }
  
  ## Functions to set and get the prior state of the matrix.
  setstate <- function(y) {
    matrixstate <<- y
  }
  getstate <- function() { 
    if(exists("matrixstate")) {
      return (matrixstate)
    }
    
    return (NULL)
  }
  
  ## Create the vector containing the functions.
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse,
       isIdentical = isIdentical,
       setstate = setstate,
       getstate = getstate)
}


## This function calculates the inverse of a matrix, by looking if the matrix has changed or not.
## If the matrix has not changed, it pulls the inverse from the cache, else, 
## it calculates the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ##Verify if the current matrix state is different from the prior state.
    if(x$isIdentical()) {
      message("Getting cached data for the inverse of the matrix")
      return(inv)
    }
  }
  data <- x$getmatrix()
  ##Calculate the inverse
  inv <- solve(data)
  ##Set the inverse.
  x$setinverse(inv)
  ##Set the current matrix state.
  x$setstate(data)
  inv  
}
