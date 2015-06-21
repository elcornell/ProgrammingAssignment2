## Pair of functions to permit caching of a matrix inverse
## in order to reduce redundant calculation

## Function to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  #set value of matrix
  
  setmat<- function(y) {
    
    x <<- y
    inv<- NULL
    
  }
  
  #get value of matrix
  getmat <- function() x
  
  #set value of inverse
  setinv <- function(invy) inv <<- invy
  
  #get value of inverse
  getinv <- function() inv
  
  #return list
  
  list(setmat = setmat, getmat = getmat, setinv = setinv, 
       getinv = getinv)
  
}


## cacheSolve computes the inverse of the matrix object returned
## by makeCacheMatrix function defined above. If the inverse has
## been calculated and the matrix is unchanged, it will return
## the value of the inverse stored in the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #check if inverse is already calculated
  
  if(!is.null(inv)) {
    
    #if yes, get inverse from cache and skip computation
    
    message("getting cached data")
    return(inv)
  }
  
  #else, calculate inverse of matrix and set value of inverse in cache
  #via setinverse function
  
  mymat <- x$getmat()
  inv <- solve(mymat,...)
  x$setinv(inv)
  inv
  
}
