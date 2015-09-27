## This set of function caches the inverse of a matrix and returns the cached inverse if it exists.

## This function creates a special "vector" which is really just a list of four functions to (1)set the value of the matrix (2)get the value of the matrix (3)set the value of the inverse (4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
  x<<-y
  i<<-NULL
}
get<-function() x
setinverse<-function(solve) i<<-solve
getinverse<-function() i
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#This function calculates the inverse of a matrix. Before it does that, it checks to see if the value of the inverse is stored in the cache. If so, it gets the value of the invserse from the cache. Otherwise, it calculates the inverse and set the value of the inverse.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i ## Return a matrix that is the inverse of 'x'
}

##Sample output:
##> z<-matrix(c(1,3,2,4),nrow=2,ncol=2)
##> zvec<-makeCacheMatrix(z)
##> cacheSolve(zvec)
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> cacheSolve(zvec)
##getting cached data
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
