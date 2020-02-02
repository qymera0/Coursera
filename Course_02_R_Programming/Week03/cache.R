## Put comments here that give an overall description of what your
## functions do

## This function set the matrix inverse, get the matrix, set the inverse when it is calculated and get the inverst when is needed.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse # Change the variable "inv" that is ourside 'setinv' environment
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This funcion check first if the inverse already exists. If so, it prints the cached. If no, it calcultes the inverse of the matrix, change the original value and pints it

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) { # Check if inv is not null
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # Get the original matrix
  inv <- solve(data, ...) # Calculates the inverse
  x$setinv(inv) # Chage the value of cached matrix on makeCacheMatrix
  inv
  
}

# OBS: teste with matrix x <- matrix(c(7, 9, 3, 4, 10, 68, 5, 7, 12), nrow = 3, ncol = 3)