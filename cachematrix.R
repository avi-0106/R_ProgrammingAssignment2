## The main objective of the "makeCacheMatrix" function is to
## create and manage the matrix while the "cacheSolve" function
## is to find the Inverse Matrix of the specified Matrix or find 
## the cached data of the Inverse Matrix.



## The following function creates the matrix and also stores 
## a few functions that helps getting and setting the values 
## the matrix and the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(x_mat){
    x <<- x_mat
    x_inv <<- NULL
  }
  get <-function() x
  setInv <- function(inv) x_inv <<- inv
  getInv <- function() x_inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The following function takes an invertible matrix
## It either find the cached Inverse Matrix and returns it
## Or calculate the inverse of the matrix and returns it

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInv()
  if(!is.null(x_inv)){
    message("getting cache Inverse matrix")
    return(x_inv)
  }
  x_mat <- x$get()
  x_inv <- solve(x_mat)
  x$setInv(x_inv)
  x_inv
}
