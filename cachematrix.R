## These functions demonstrate getter and setter method by
## Showing how to inverse a matrix and cache it to potentially save computing time

## the makeCacheMatrix function creates an object that allows us to store 
## an initial matrix as well as an inverse matrix,
## and allows us to access these objects using getter and setter functions

makeCacheMatrix <- function(my_matrix = matrix()) { # a matrix gets passed to the function
  invM <- NULL #initializing the inverse matrix
  set <- function(y) { # after the object (the matrix) is created, it can be set to a new value using the set function
    my_matrix <<- y # set matrix to new value
    invM <<- NULL # reset the inverse matrix
  }
  get <- function() my_matrix # return the non-inveted matrix
  setinverse <- function(inverseMatrix) invM <<- inverseMatrix # cache the inverted matrix
  getinverse <- function() invM #return the cached matrix; will be null if it was not cached or if a new matrix was set (a non-inverted one)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the following function allows to invert a matrix
## if the matrix was inverted before, a cached version is pulled
## the function assumes the matrix can be inverted

cacheSolve <- function(matrixToInvert, ...) {
  # the matrix to invert is passed into the function
  # the object passed into the function has the ability to cache
  inverseMatrix <- matrixToInvert$getinverse() ## get the cached matrix (NULL if does not exist yet)
  
  if(!is.null(inverseMatrix)) { # if the cached matrix exists
    message("getting cached data") # send message to the console
    return(inverseMatrix) #return the matrix and exit the function
  }
  #only executed if no chache (due to the return statement)
  initialMatrix <- matrixToInvert$get() #returns the original matrix
  inverseMatrix <- solve(initialMatrix) #inverse the matrix
  matrixToInvert$setinverse(inverseMatrix) #cache the inverse matrix  by using the setter function of the originatl object
  # not that initialMatrix is not an object that has the getter and setter fuctions
  inverseMatrix #return the inverse matrix
}
