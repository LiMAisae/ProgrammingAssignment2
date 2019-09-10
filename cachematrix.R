## This 2 functions below allow to create a special matrix and 
## cache its inverse. If the inverse computing is recalled
## repeatedly, the cached inverse is automatically outputed.

## The first function, `makeCacheMatrix` creates a special "matrix",
## which is really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set, get=get, 
       getinverse=getinverse,
       setinverse=setinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it `get`s the inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
