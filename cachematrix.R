## MakeCacheMatrix: Receives a matrix or no argument, and creates a vector containing the functions to:
## set() the value of the matrix
## get() the value of the matrix
## setInv() set the value of the Inverse of the Matrix
## getInv() get the value of the Inverse of the Matrix

##Once Created, the vector may be accessed using the $ symbol, followed by the name of the function to use
## Example:
## M <- MakeCacheMatrix() #Create the vector
## M$set(matrix(c(1,2,4,3), 2,2)) #Sets the value of the matrix.
## M$get() #returns the stored value of the matrix (returns NULL if the matrix hasn't been defined.)

makeCacheMatrix <- function(M = matrix()) {
  MInv <- NULL
  set <- function(Y) {
    M <<- Y
    MInv <<- NULL
    }
  get <- function() M
  setInv <- function(setMInv) MInv <<- setMInv
  getInv <- function() MInv
  list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve: This function receives a vector and verifies if the inverse of the matrix stored has been already set.
## If it has, then it returns the value of the stored Inverse, saving time by only retrieving the values instead of
## recalculating them. If the value has not been set, it calculates it, and then caches it so that we may reuse the 
## value.

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'M'
  MInv <- M$getInv()
    if(!is.null(MInv)) {
    message("getting cached Inverse.")
    return(MInv)
  }
  
  data <- M$get()
  MInv <- solve(data)
  M$setInv(MInv)
  MInv
}
