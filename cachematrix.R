## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores a matrix X

## cacheSolve shows the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(Y){
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}
## Write a short comment describing this function
## cacheSolve uses corpcor
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(require("corpcor")){
    print("corpcor is loaded.")
  } else {
    print("installing corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("couldn't install corpcor")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse will be computed")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  inverse
}

