## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cachedResult <- NULL
  set <- function(y){
    x <<- y
    cachedResult <<- NULL
  } 
  get <- function() x
  setCachedResult <- function(cachedResult) cachedResult <<- cachedResult
  getCachedResult <- function() cachedResult
  list(
    set = set,
    get = get,
    setCachedResult = setCachedResult,
    getCachedResult = getCachedResult
  )
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getCachedResult()
  if( !is.null(inverse) ){
    message("getting chache data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setCachedResult(inverse)
  return(inverse)
  
}
