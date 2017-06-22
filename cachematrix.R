## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Making a special " matrix " object that can cache its inverse.. 
# Set Inversion Matrix into inver

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL 
  set <- function(y){
    
    x <<- y
    inver <<- NULL 
    
  }
  
  get <- function() x 
  setinver <- function(iv) inver <<- iv
  getinver <- function() inver
  
  list(set = set, get = get , 
       setinver = setinver,
       getinver = getinver)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
# cacheSolve 
# computes the inverse of special " matrix " returned by makeCacheMatrix above. 
# Refer to examples of 'cachemean' 
  
  inver <- x$getinver()
  if(!is.null(inver)){
    message("Getting cached data")
    return (inver)
  }
  
  inputMatrix <- x$get()
  inver <- solve(inputMatrix, ...)
  x$setinver(inver)
  inver
  
  
  
}
