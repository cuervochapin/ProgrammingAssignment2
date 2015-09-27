## This first part creates and stores the functions that help
## validate if the inverse has been calculated before
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function(solve)
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## It stores the matrix on a variable and will store it

## This function calculates the inv if it hasn't been calculated before
## If this is tha case it just returns the stored info
        
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
