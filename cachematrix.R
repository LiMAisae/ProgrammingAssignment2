## This 2 functions below allow to create a special matrix and 
## cache its inversion. If the inversion computing is recalled
## repeatedly, the cached inversion is automatically outputed.

## The first function, `makeCacheMatrix` creates a special "matrix",
## which is really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inversion
##4.  get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinversion<-function(inversion) inv<<-inversion
  getinversion<-function() inv
  list(set=set, get=get, 
       getinversion=getinversion,
       setinversion=setinversion)
}


## The following function calculates the inversion of the special "matrix"
## created with the above function. However, it first checks to see 
## if the inversion has already been calculated. If so, it `get`s the inversion
## from the cache and skips the computation. Otherwise, it calculates 
## the inversion of the data and sets the value of the inversion in the cache 
## via the `setinversion` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinversion()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinversion(inv)
  inv
}
