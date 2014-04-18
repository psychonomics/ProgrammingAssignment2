## These two functions can be used to cache the inverse of a matrix, and retrieve the inverse (or calculate it if it does not exist)

## makeCacheMatrix() accepts a matrix (empty by default), and returns a "special matrix" list that contains methods for 
## (1) setting the matrix (2) getting the matrix (3) setting the inverse of the matrix and (4) getting the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
# makeCacheMatrix() accepts a matrix (empty by default) 
  
  # Assign NULL to the inverse. This allows the user to call getinverse() without error if the inverse has not been cached
  i <- NULL
  
  # Method 1. Assign the matrix to x, so that it can be retrieved. Set the inverse to NULL (the original matrix has changed)
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Method 2. Return the matrix x when called
  getMatrix <- function() x
  
  # Method 3. Store the matrix inverse i.  The function acts as a store of the given value and should not be called directly
  setinverse <- function(solve) i <<- solve
  
  # Method 4. Return the matrix inverse i.  
  getinverse <- function() i
  
  # Return a list of methods (1) set the matrix (2) get the matrix (3) set the matrix inverse and (4) get the matrix inverse 
  list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse)  

}


## cacheSolve accepts a list created by makeCacheMatrix, and returns a matrix which is the inverse of the original matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

## Return the inverse matrix if it exists.  Do not recompute.  
  
  # Retrieve the inverse of the original matrix (this may be NULL)
  i <- x$getinverse()

  # If the inverse exists, return it and exit
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
## Otherwise, calculate the inverse of the original matrix and cache it 

  # Retrieve the original matrix, and assign to data
  data <- x$getMatrix()
  
  # Compute the inverse of the original matrix, and assign to m
  m <- solve(data, ...)
  
  # Cache the inverse of the original matrix
  x$setinverse(m)
  
  # Return the inverse matrix
  m

}
