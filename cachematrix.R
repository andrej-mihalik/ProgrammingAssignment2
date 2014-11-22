## Definition of makeCacheMatrix and cacheSolve functions to calculate matrix inverse, store the result
## and return it if it is to be recomputed instead of calculating it again, thus saving time and resources


## Creates a list of functions to set the value of matrix, get the value of matrix, 
## set the inverse matrix and get the inverse matrix using the matrix supplied as its argument

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Using the list of functions specified above, this function either calculates the inverse matrix
## (if the matrix supplied is invertible) or returns the already calculated value from cache if 
## it has been calculated before

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
