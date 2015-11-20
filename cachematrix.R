##
## makeCacheMatrix
##
## Create a special "vector" which is really a function to
##
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the inverse of the vector
## 4. get the inverse of the vector
##


makeCacheMatrix <- function(x = matrix()) 
{
  ## set to NULL to be sure
  
  inv <- NULL
  
  ##
  ## 1. Set the value of the vector
  ##
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  ##
  ## 2. Get the value of the vector
  ##
  get <- function() X
  
  ##
  ## 3. Set the inverse of the matrix
  ##
  setinverse <- function(inverse) inv <<- inverse
  
  ##
  ## 4. Get the inverse of the matrix
  ##
  getinverse <- function() inv
  
  ##
  ## List the values 
  ##
  list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##
## Compute the inverse of a matrix 
##

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
