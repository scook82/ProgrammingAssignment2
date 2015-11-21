## Functions to solve for the inverse of a matrix, storing the value in cache so we don't need to compute twice
## useful for matrices that are large and take a long time to solve.
## example call: 
#B = matrix( c(1, 2, 3, 4),nrow=2,ncol=2) 
#cacheSolve(makeCacheMatrix(B))

## Function makeCacheMatrix: initializes a variable m that stores the cached result of a matrix
##   returns a list of accessor method functions and inverse functions

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize m
  m <- NULL

  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Function cacheSolve: Checks to see if the inverse of the matrix has been calculated, if so returns 
##   cached value, otherwise calculates and sets cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the inverse of the matrix
  m <- x$getinverse()
  
  # if the inverse was already calculated, return stored result (m)
  if(!is.null(m)){
    #get cached data and return (without doing the calculation)
    return (m)
  }
  # if no cache found, solve for the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
# Test calls
#B = matrix( c(1, 2, 3, 4),nrow=2,ncol=2) 
#cacheSolve(makeCacheMatrix(B))
