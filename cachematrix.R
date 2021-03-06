## These two functions can be used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ## Caches matrix x, initializes cached inverse matrix m with NULL.
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## Returns original matrix x.
  get<-function() {x}
  ## Caches inverse matrix m.
  setinverse<-function(inverse) {m<<- inverse}
  ## Returns cached inverse matrix m (or NULL).
  getinverse<-function() {m}
  message("[INFO] Special matrix has been initialized which can cache its inverse. \nFunctions are set(...), get(), setinverse(...) and getinverse()")
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by function 'makeCacheMatrix'. 
## If the inverse matrix has already been calculated (and the matrix has not changed), the function
## 'cacheSolve' will retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  
  ## Checks if an inverse matrix has already been cached.
  ## m will be NULL if the inverse matrix has not been calculated and cached before
  ## or the original matrix has been changed.
  if(!is.null(m)){ 
    message("[INFO] Getting cached inverse matrix.")
    return(m)
  }
  matrix<-x$get()
  ## Calculates inverse of a square matrix.
  if(ncol(matrix)==nrow(matrix)){
    m<-solve(matrix, ...)
    ## caches inverse matrix m
    x$setinverse(m)
  } else {
    warning("Cannot inverse non-square matrix.")
  }
  m
}
