## Basically this file is based on the given example, both the structure of the functions and the 
## explanations. The first function makes a cache for storage of the results, it's like
## a small bag containing matrix ever appeared and its corresponding inverse, and when we 
## need the inverse, we call the second function. If the result already exists, "get" funcion
## helps to retrieve the value; otherwise we caculate the value and store it in cache by "set"
## function

## makeVector` creates a special "vector", which is really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL;
      set <- function(y){
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


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        
       i <- x$getinverse()
       if(!is.null(i)){
             message("getting cached data")
             return(i)
       }
       data <- x$get()
       i <- solve(data,...)
      x$setinverse(i)
      i
}
