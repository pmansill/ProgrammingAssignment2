## We want to avoid some costly computation when inverting a matrix by caching the inverse of a matrix rather than compute it repeatedly.
## The first function creates an object that can cache the inverse of a matrix. The second function computes the inverse of the object returned by the first function. If the inverse has already been calculated, it then retrieves the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)i<<-solve
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## The first function creates an object that can cache the inverse of a matrix.It assigns the input to the parent environment and null to i, defines getters and setters and names list elements to use the $ form of the extract operator

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached inversed")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
## Return a matrix that is the inverse of 'x'
}
