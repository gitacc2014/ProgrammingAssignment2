## The functions makeCacheMatrix and cacheSolve help cache the results of Inverse and regenerate as an when required.


#Theoritically makeCacheMatrix is something like a Matrix object with members methods setters and getters
makeCacheMatrix <- function(x = matrix()) {
	#Condition added to confine the matrices to square
	if(!is.matrix(x) || nrow(x)!=ncol(x))
		{
			return("Please enter square matrix only and retry")
		}
		else{
			MatInv <- NULL
			#sets the matrix
			setMat <- function(y)
			{
					if(!is.matrix(y) || nrow(y)!=ncol(y))
				{
					return("Please enter square matrix only and retry") 		
				}
					x <<-y
					MatInv <<- NULL
			}
		#gets the matrix
		getMat <- function() x
		#sets the matrix Inverse if required
		setMatInv <- function(inverse) MatInv <<- inverse
		#gets the matrix Inverse if required
		getMatInv <- function() MatInv
    list(setMat=setMat,getMat=getMat,setMatInv=setMatInv,getMatInv=getMatInv)
    }
}


## CacheSolve checks if the inverse of the matrix had been calculated and cached, if not it calculates and sets it

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
