### Create two functions that will create a special matrix and an object that can compute and cache its inverse.

## Make cachematrix, define function and clear cache. 
###Get value of matrix by defining function. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   
  ### Defines the function by setting the value of the matrix and clearing cache.
  set <- function(y) {
    x <<- y   ### Sets value
    m <<- NULL ### Clear cache 
  }
  ### Get value of matrix by using definition of function. 
  get <- function()x
  ### Set inverse by defining function.
  setInverse <- function(inverse) m <<- inverse 
  ### Get inverse from definition of function
  getInverse <- function()m 
  ### Return list with 4 functions 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


### Compute inverse of matrix returned by makeCacheMatrix. 
### Return inverse of matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()   ### Returns cached value for inverse
  ### If cache not empty, it can be returned 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  ### If cache is empty, it must be calculated, cached, and returned. 
  data <- x$get() ### Get value of matrix
  m <- solve(data) ### Compute inverse
  x$setInverse(m)  ### Cache result 
  m     ### Return inverse 
}



