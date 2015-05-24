### R Programmging Assignment 2: calculating the inverse of a Matrix 

## The functions makeCacheMatrix() and cacheSolve() calculate the inverse of a matrix.
## However, since calculating the inverse of a (large) matrix can require quite some 
## computational power, the function cacheSolve() only calculates the inverse if it hadn't
## been calculated before. Otherwise, it reports the inversed saved in the cache from 
## previous computation

## The function makeCacheMatrix() houses a list four different functions. 
## The input to the function is a matrix. The function saves matrix and its inverse (if 
## already computed). Calling $set(), the function can overwrite the matrix saved. $get()
## reports the matrix saved. $setinverse() and $getinverse() do similar operation for the
## matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
      dim <- numeric()
      dim[1] <- dim(x)[1]
      dim[2] <- dim(x)[2]
      ## by assumption of the assignment, any input matrix is a square matrix (is invertible)
      ## however, to avoid errors in the beginning of the function if the assumption isn't met,
      ## the function saves the dimensions separately
      i <- matrix(, nrow = dim[1], ncol = dim[2])
      
      set  <- function(y) {
            x <<- y
            dim[1] <- dim(x)[1]
            dim[2] <- dim(x)[2]
            i <<- matrix(, nrow = dim[1], ncol = dim[2])
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function () i
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## The function cacheSolve() does two things: if its input - an object in which 
## makeCacheMatrix() is saved - already contains the calculated inverse of the matrix
## it reports the inverse. If the inverse has not been calculated yet, cacheSolve()
## calculates and saves the inverse of the matrix constained in cacheSolve().

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      
      if(all(!is.na(i))){
            message("retrieving cached inverse")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
x$setinverse(i)
i
}
