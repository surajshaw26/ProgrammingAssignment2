## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  Inverse_matrix<-NULL
  set<-function(y){
    x<<-y
    Inverse_matrix<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) Inverse_matrix<<-inverse
  getinverse<- function() Inverse_matrix
  list(get=get,set=set,setinverse=setinverse,getinverse=getinverse)
}



## This returns a matrix that is inverse of the input matrix 'x' created by the function makeCacheMatrix. If the inverse is already calculated, it should retrieve the same from cache. 

cacheSolve<-function(x,...){
  Inverse_matrix<-x$getinverse()
  if(!is.null(Inverse_matrix)){
    message("getting cached data")
  return(Inverse_matrix)
  }
  data<-x$get()
  Inverse_matrix<-solve(data,...)
  x$setinverse(Inverse_matrix)
  Inverse_matrix
}
