## This function creates a special matrix and turns a list consisting of four
## functions in a local environment.
makeCacheMatrix <- function(x = matrix()){
	invx <- NULL          ## 'invx' denotes the inverse of matrix x. 
	set <- function(y) {  ##This is the first function which states the matrix.
		x <<- y
		invx <<- NULL
	}
	get <- function()  ## This is the second function which returns the matrix.
		x
	setsolve <- function(solve) ## This is the third function which sets the inverse of the matrix.
		invx <<- solve
	getsolve <- function() ## This is the fourth function which returns the inverse of the matrix.
		invx
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}



## This matrix creates a cache on the inverse of a matrix created by the
# function "makeCacheMatrix".
cacheSolve <- function(x, ...) {
        invx <- x$getsolve()
        if(!is.null(invx)) {  ## Here, the function returns the inverse of the matrix if it is not null. 
                message("getting cached data")
                return(invx)
        }
        data <- x$get()   ## If it is null, it gets the matrix and calculates its inverse and then sets its value.
        invx <- solve(data, ...)
        x$setsolve(invx)
        invx
}
