## The following two functions are used to cache the inverse of a matrix
##in order to reduce the time of computation. The matrix is assume invertible. 

## The following function makeCacheMatrix creates a special "matrix", which is a list containing a function to:
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  c<-NULL
  set<-function(y){
    x<<-y
    c<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) c<<-inverse
  getinverse<-function() c
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve compute the inverse of special "matrix" created
## with makeCacheMatrix. It first checks to see if the inverse of the matrix
## has already been calculated. In this case, it gets the inverse from the cache
##and skips calculus. Otherwise, it computes the inverse of the data and sets
##the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c<-x$getinverse()
  if(!is.null(c)){
    message("getting cached data.")
    return(c)
  }
  mat<-x$get()
  c<-solve(mat, ...)
  x$setinverse(c)
  c
}

