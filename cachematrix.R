## The two function work together to calculate the inverse of a matrix (matrix must be inversable).
## When ran a first time it will calculate the inverse of the passed matrix and store result in a variable.
## When ran the second time it skip the calculation step and return the stored results.
## To use the functions properly use following steps:
## 1. create makeChacheMatrix object by passing a matrix as an arugment e.g. mk <- makeCacheMatrix(x(1,0.25,0.25,1))
## 2. run cashSolve function by passing mk object as an argument e.g. cashSolve(mk)
##    
## 


## Write a short comment describing this function:
## This function creates an object with four methods and a varible that stored the value of the inverse function:
## Method get returns the value of the matrix passed as the argument when object was created
## Method set creates a new matrix object and replaces the old matrix object passed in the argument 
## Method setinv calculates inverse of the matrix and stores it in inv variable
## Method getinv returns the value of inv variable
## the function returns a list of methods that can be called when object is created

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
	  set <- function(y) {
		x <<- y
		inv <<- NULL
	  }
	  get <- function() x
	  setinv <- function(solv) inv <<- solv
	  getinv <- function() inv
	  list(set = set, get = get,
		   setinv = setinv,
		   getinv = getinv)
}


## Write a short comment describing this function:
## Return a matrix that is the inverse of 'x'
## 1. the function called the method getinv available from makeCacheMatrix object.
## 2. if return value of 'i' is not null (!is.null(i)) then it will find and return the cached value
## 3. if return value of 'i' is null then it will calcuate the inverese and return the inverse value

cacheSolve <- function(x, ...) {
       
		  i <- x$getinv()
		  if(!is.null(i)) {
			message("getting cached data")
			return(i)
		  }
		  data <- x$get()
		  i <- solve(data, ...)
		  x$setinv(i)
		  i
}
