##-----------------------------------------------
## makeCacheMatrix: this function creates a
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      #set starting inverse matrix value to NULL
      invMatrix<-NULL
      
      #a function to set a new matrix
      set<-function(y = matrix()){
            x<<-y
            invMatrix<<-NULL
      }
      
      #a function to get the existing matrix
      get<-function() x
      
      #a function to calculate the inverse matrix
      calcInverse<-function(){
            invMatrix<<-solve(x)
      }
      
      #a function to get the inverse matrix
      getInverse<-function() invMatrix
      
      #return the list of functions
      list(set=set, get=get, 
           calcInverse=calcInverse, getInverse=getInverse)
}

##-----------------------------------------------
## cacheSolve: this function computes the inverse
## of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
      ##get the inverse matrix value
      invMatrix<-x$getInverse()
      
      ##if inverse matrix already calculated - return it
      ##from the cache
      if(!is.null(invMatrix)){
            message("getting cached inverse matrix")
            return(invMatrix)
      }
      
      ##if inverse matrix is not yet calculated - calculate
      ##it now and return
      message("calculating inverse matrix")
      x$calcInverse()
      x$getInverse()
}
