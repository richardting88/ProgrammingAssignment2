## The folloiwng two functions can be used to cache the inverse of a matrix

##makeCacheMatrix creates a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL    
  }
  get <- function() x
  setInv <- function(inverseM) invM <<- inverseM
  getInv <- function() invM
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) 
}

## cacheSolve calculates the inverse matrix of the matrix created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the
## mean from the cache and skips the compuation. Otherwise, it calculates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invM <- x$getInv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setInv(invM)
  invM
}
