## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#Condition added to confine the matrices to square
	if(is.matrix(x) && nrow(x)!=ncol(x))
		{
			print("Please enter square matrix only and retry")
			return()
		}
		MatInv <- NULL
		setMat <- function(y){
			x <<-y
			MatInv <<- NULL
		}
		getMat <- function() x
		setMatInv <- function(inverse) MatInv <<- inverse
		getMatInv <- function() MatInv
    list(setMat=setMat,getMat=getMat,setMatInv=setMatInv,getMatInv=getMatInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        MatrixInverse <- x$getMatInv()
        mat <- x$getMat()
        if(!is.null(MatrixInverse))
        {
        	message("Getting cached inverse")
        	return(MatrixInverse)
        }
        
        MatrixInverse <- solve(mat)
        x$setMatInv(MatrixInverse)
        MatrixInverse
}
