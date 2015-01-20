## MakeCacheMatrix: Receives a matrix, in order to assign it to 

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
  MInv <- M$getInv()
    if(!is.null(MInv)) {
    message("getting cached data")
    return(MInv)
  }
  
  data <- M$get()
  MInv <- solve(data)
  M$setInv(MInv)
  MInv
}
