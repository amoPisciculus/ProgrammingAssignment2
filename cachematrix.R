## Put comments here that give an overall description of what your
## functions do

## Function returns a special matrix with 4 methods that set the matrix,
## get the matrix, and turn into a cached value for easy access 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(mat) m <<- mat
  getMatrix <- function() m
  list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)

}


##  Function returns inverse of matrix input in object: computes if 
## not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
