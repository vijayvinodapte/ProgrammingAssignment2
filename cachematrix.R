## makeCacheMatrix(): it prepares a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  i<- NULL
  set <- function(k) {
    x <<- k
    i <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 
 
## cacheSolve(): it calculates the inverse of the matrix returned by function makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it
## gets the inverse directly from the cache.
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  i <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(i)) {
    message("Bring cached data")
    return(i)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  i <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(i)
  i
}