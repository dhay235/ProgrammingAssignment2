## Below are two functions that are used to create a special object that
## stores a matrix and cache's its inverse. 

## The first function, makeCacheMatrix creates a list which contains
## a function to: 
## 1. Set the matrix.
## 2. Get the matrix. 
## 3. Set the inverse of the matrix. 
## 4. Get the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y){
    # Here "<<-" is used to assign a value to an object
    # in an environment that is different from the current environment.
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  set_minverse <- function(inverse) minverse <<- inverse
  get_minverse <- function() minverse
  list(set = set, get = get, 
       set_minverse = set_minverse,
       get_minverse = get_minverse)
}


## The second function, cacheSolve calculates the inverse of the special
## matrix created with the above function. However, it first checks to 
## see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the inverse in the cache via the 
## set_minverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inverse <- x$get_minverse()
  # "if" statement checks if the inverse has already been calculated.
  # If so, retrieves cached inverse and skips computations of the inverse. 
  if(!is.null(m_inverse)){
    message("getting cached data")
    return(m_inverse)
  }
  # If no inverse cached, computes inverse below. 
  data <- x$get()
  m_inverse <- solve(data,...)
  # Sets the inverse in the cache using the set_inverse function. 
  x$set_minverse(m_inverse)
  # Return matrix inverse. 
  m_inverse
}
