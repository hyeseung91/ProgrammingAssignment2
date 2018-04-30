## Assuming matrices are always invertible,
# the function caches the inverse of the matrix by caching 

## makeCacheMatrix creates a special matrix with a list containing a function doing the following: 
# 1) sets matrix 2) gets matrix 3) sets inverse of matrix 4) gets inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  } 
  get<-function() x
  set_inverse<-function(solve) m<<-solve
  get_inverse<-function() m 
  list(set=set, get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}

## cacheSolve function first checks if inverse of matrix already exists, and prints that matrix if already calculated, and if not, 
#it will calculate inverse of matrix using set_inverse function

cacheSolve <- function(x, ...) {
  m<-x$get_inverse() 
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  m
}
