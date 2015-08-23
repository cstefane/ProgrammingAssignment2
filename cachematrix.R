## The following function are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to:
## 1.set the value of the invertable matrix -> set
## 2.get the value of the invertable matrix -> get
## 3.set the inverse value of the matrix -> setinv
## 4.get the inverse value of the matrix -> getinvb


makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL         # initialize the matrix inverse
  set <- function(y) {
          x <<- y      # assigning the value to x in other environment
          minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv 
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
        

}


## This function computes the inverse of the special "matrix"   
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {

        minv <- x$getinv()
        
        # if the inverse is already calculated, we get it from cache
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        } else {        # not calculated...
                
                # calculate and store the inverse to cache
                mat <- x$get()
                minv <- solve(mat)
                x$setinv(minv)
        }
        
        minv
}

