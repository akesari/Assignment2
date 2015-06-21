## Functions to calculate and cache the inverse of matrix
## 
## Running Code Example
## a <- makeCacheMatrix(matrix(c(1,2,20,21),2,2))
## cacheSolve(a)
## 
## makeCacheMatrix is a function to create a special "matrix"
## and store cached value of the inverse of the matrix
## It contains the following functions
## set          set the value of the matrix
## get          get the value of the matrix
## setInverse   set the value of the inverse matrix
## getInverse   get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(solve){
    m <<- solve
  }
  getInverse <- function(){
    m
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Following code calculates the inverse of the
## matrix created above and reuses cached result
## if available
## cacheSolve returns the the value of the inverse
## of the matrix created using makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}

