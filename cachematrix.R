## the first function creates a special matrix object 
#and caches the inverse of it
#The second function compute the return of the first function 

## this function creates a special matrix object which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  SetMatrix <- function( matrix ) {
    x <<- matrix
    m <<- NULL
  }
  
  getMatrix <- function() {
    
    x
  }
  
  setInverse <- function(inverse) {
    m <<- inverse
  }
  
  
  getInverse <- function() {
    m
  }
  list(set = SetMatrix, get = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

}


##computing the returned matrix by the makeCacheMatrix function
##checks if the inverse is calculated and retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$getMatrix()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
