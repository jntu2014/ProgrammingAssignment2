## This function returns a list of four functions

## The returned funcions are to set the matrix, get
## the matrix, calculate the inverse of the matrix,
## get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(solve) m<<-solve
  
  getinverse<-function() m
  
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)
}

## The function checks the cache to see if the inverse
## of the matrix has bee calculated. If calculated, 
## returns it from the cache. Otherwise claculates
## the inverse
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  matrix<-x$get()
  
  m<-solve(matrix, ...)
  
  x$setinverse(m)
  
  m
}
