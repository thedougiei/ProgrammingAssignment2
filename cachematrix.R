## This is a programming assignment for the following modUle
## https://class.coursera.org/rprog-004/human_grading/view/courses/972139/assessments/3/submissions

## there are 2 functions supplied
##
## makeCacheMatrix() and cacheSolve()
##
##
## To execute and test, the sequence of steps would be
##
##		 m<- makeCacheMatrix( )
## 		 m$set( .. your square matrix ...)
## 		 m$get()
##  	 cacheSolve( m )
##  
## 		 and again... should be cached now
##  	 cacheSolve( m )



## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special 'matrix', which is really a list containing a function to
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse
##	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
	get <- function() x
	
	setinverse <- function(solve) m <<- solve(x)
	getinverse <- function()  m
	list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function returns the inverse of a square matrix
## The inverse calculation is only executed if there is no previously cached value

## a message 'getting cached data' is returned to indicated a cached value was used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse()
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}









