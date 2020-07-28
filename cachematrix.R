## These 2 functions will return the inverse of a matrix. 
## For a new matrix, the inverse will be calculated, but for the same matrix, the 
## inverse will be cached and recalled without wasting repeat calculation time.

## To use these functions, define a square matrix, eg. m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## Then define myMatrix <- makeCacheMatrix(m1)
## Then call cacheSolve(myMatrix)
## The first time you do this, the inverse will be calculated. Subsequent times, the inverse will be recalled.

## The first function, makeCacheMatrix, creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function()m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function, cacheSolve, returns a matrix that is the inverse of x.
## It uses the first function to do this, either calculating the inverse or returning the cached inverse if it had previously been calculated.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
