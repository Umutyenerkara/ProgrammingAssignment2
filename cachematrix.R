#This function creates a special matrix object containing four functions: set(), get(), setinverse(), and getinverse(). 
#The resulting object can be inputted into CacheSolve function to return its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}

#This function takes the special object created by makeCacheMatrix function and returns its inverse by using solve function.
#If m is not null, returns cached invertible matrix. Otherwise it gets the original matrix data, uses solve function then returns. 


cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}

