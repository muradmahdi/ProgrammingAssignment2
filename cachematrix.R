## makeCacheMatrix: return list of function as follow:
## 1- set(x) 	: set the value of the matrix ‘x’
## 2- get() 	: return value of the matrix ‘x’
## 3- setInv(x) : set the value of the matrix inverse‘x’
## 4- getInv() 	: return value of the matrix inverse ‘x’


## this function will create a matrix object that can cache its ## inverse

makeCacheMatrix <- function(x = matrix()) {
	## Initialization of Matrix Inverse variable
	inv <- NULL
	
	## set function for matrix
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	## get function for matrix
	get <- function () x

	## set function for matrix inverse
	setInv <- function (inverse) inv <<- inverse

	## set function for matrix inverse
	getInv <- function() inv

	## Return list of functions for Matrix and its inverse
	list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacheSolve: This function will compute the a matrix inverse, 
## if inverse is cached it will return it
## if inverse is not cached it will compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	## get matrix inverse value
	inv <- x$getInv()

	## checking if inverse is cached
	if(!is.null(inv)){
	return(inv)
	}
	

	## get matrix value	
	data <- x$get()

	## compute inverse and storing it.
	inv <- solve(data, ...)
	x$setInv(inv)
	
	## return inverse value
	inv
}
