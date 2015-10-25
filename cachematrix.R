##Together these two functions calculate the inverse of an invertible matrix and cache the solution.
##This will save time in repeat calculations, for if the matrix has not changed, the inverse
##can be looked up in the cache instead of recalculated.



##This function creates a list of elements allowing for getting/setting of
##the matrix of interest and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #Initializes a Variable helping to set the inverse. 
  
  m <- NULL
  
  #Sets the matrix information outside current enivironment. 
  
  set <- function(y) {
    
    x <<- y
    m <<- NULL
    
  }
  
    #Gets the matrix information.
  
  get <- function() x
  
    #Sets the inverse of the matrix outside current environment.
  
  setinverse <- function(inverse) m <<- inverse
  
  #Gets the inverse of the matrix.
  
  getinverse <- function() m
  
  #Returns a list with all 4 pieces of information stored.
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}



##This function calculates the inverse of the invertible matrix created in the makeCachematrix function,
##If the inverse has already been calculated, however, the function will look up the solution from the cache
##and skip the computation.

cacheSolve <- function(x, ...) {
  
  #Attempt to look up the inverse for the matrix in the cache.
  
  m <- x$getinverse()
  
  #If it exists, return the solution, provide a note of this, and skip the inverse computation.
  
  if(!is.null(m)){
    
    message("getting cached data")
    
    return(m)
  }
  
  #If it does not exist, get the contents of the matrix.
  
  data <- x$get()
  
  #Calculate the inverse of the matrix.
  
  m <- solve(data,...)
  
  #Cache the solution.
  
  x$setinverse(m)
  
  #Return the calculated inverse.
  
  m
}
