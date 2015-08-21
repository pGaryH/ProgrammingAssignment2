## Put comments here that give an overall description of what your
## functions do
##    Based on the makeVector and cachemean functions in example for this assignment.
##    Essentially, replace vector with matrix and solve for inverse instead of finding mean.
##    Just a disclaimer: MakeVector and cachemean were modified to create these two functions.


## Write a short comment describing this function.
##  makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatinv <- function(invMat) m <<- invMat
  getmatinv <- function() m
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatinv()
  
  if(!is.null(m)) {     #inverse is cached
    message("getting cached data")
    return(m)
  }
  
  message("inverse not cached, calculating inverse")  
  data <- x$get()       #not cached, calculate inverse
  m <- solve(data)
  x$setmatinv(m)
  m
}
