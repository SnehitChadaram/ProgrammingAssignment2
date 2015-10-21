## Put comments here that give an overall description of what your
## functions do

## To use the program, create a sample matrix and give as input to the
## "makeCacheMatrix". Now you can use the functions specified or you can
## check for the inverse using "cacheSolve" by giving the 'CacheMatrix' as input

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmat<-NULL          ##This contains the inverse matrix
  set <- function(y)    ##Some functions that can be used
  {
    x<<-y
    invmat<<-NULL
  }
  get <- function() x
  setinv <-function(solve) invmat<<- solve
  getinv <- function() invmat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This checks for the cached data if present else calculates and stores in cache
## for using it next time
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat<- x$getinv()
  if(!is.null(invmat))
  {
    message("getting cache data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}
