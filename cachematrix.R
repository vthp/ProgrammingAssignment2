## I'm using the example to try and set
## the value of a matrix.
## Essentially we are using the solve instead
## of using the mean.
## We then want to get the inverse of the
## given matrix either using the cache if
## it exists there, or computing it.

## This first function initialises m to
## NULL. We then set the function to solve
## and cache the value. set, get, setinverse
## and getinverse are then stored as a vector.
## The environment in which these caches 
## were defined can be called to use the
## passed values.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setinverse<-function(solve) m<<-solve
getinverse<-function() m
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calls elements of the vector list from
## makeCacheMatrix and can enter the original
## defining environment.
## If result is returned from the cache
## then this is noted in a messsage.
## Else, the result is returned and cached
## for use later without the computation.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}