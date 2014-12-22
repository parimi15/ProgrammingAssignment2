# The couple of functions 'MakeCacheMatrix' & 'cacheSolve' returns a matrix that 
# is the inverse of 'x'

#The function MakeCacheMatrix, creats a special matrix that consists of a list 
# containing a function to 
#1. set the value of the matrix 
#2. get the value of the matrix 
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL 
  set <- function(y) { 
    x <<- y 
    I <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) I <<- inverse 
  getinverse <- function() I 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

# The function 'cacheSolve' returns a matrix that is the inverse of the 
# special matrix 'I'
  
cacheSolve <- function(x, ...) { 
  I <- x$getinverse() 
  if(!is.null(I)) {
    message("getting cached data") 
    return(I)
  } 
  data <- x$get()
  I <- solve(data, ...) 
  x$setinverse(I) 
  I
}