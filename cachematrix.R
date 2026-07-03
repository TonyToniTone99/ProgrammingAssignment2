#Creates a matrix object that can cache its inverse

  makeCacheMatrix <- function(x = matrix()) {
    
  
      #Initialize the cache inverse as NULL since it has not been computed yet
      inv <- NULL
      
      
      #Function to replace the Matrix
      set <- function(y) {
        
        x <<- y  #Update the stored matrix
        
        inv <<- NULL  #Clear the cached inverse
        
      }
      
      #Function to return the current matrix
      get <- function(){
        x
      }
      
      #Function to store the inverse once it's computed
      setinverse  <- function(inverse){
        
        inv <<- inverse
        
      }
      
      #Function to retrieve cached inverse
      getinverse <- function() inv
        
      #Return a list containing all of the funtions
      list(
        set = set, 
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse
      )
}



  #Function computes the inverse of the matrix object
  cacheSolve <- function(x, ...) {
    
        #Checks whether the inverse has already been computed
      inv <- x$getinverse()
      
      #if inverse exists, return it immediately
      if(!is.null(inv)){
        
        message("getting cached data")
        
        return(inv)
      }
      
      
      #Otherwise, retrieve the original matrix
      data <- x$get()
      
      #Compute the inverse of matrix
      inv <- solve(data, ...)
      
      #Store the inverse in cache
      x$setinverse(inv)
      
      #Return the newly compute inverse
      inv
  
}
