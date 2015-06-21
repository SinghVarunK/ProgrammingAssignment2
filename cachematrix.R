## Description : The function acts as a getter and setter function
##             : for the matrix and inverse of a matrix. The function
##             : provides the methods for getting and setting the matrix 
##             : and the inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #Function to set the matrix in the cache
        set <- function(y){
                  x <<- y
                  inv <<- NULL
        }
        
        #Function to get the matrix from the cache
        get <- function(){
                  x
        }
        
        #Function to set the inverse of the matrix in the cache
        setInvMatrix <- function(invMatrix){
                  inv <<- invMatrix    
        }
        
        #Function to get the inverse of the matrix from the cache
        getInvMatrix <- function(){
                  inv
        }
        
        #List of the methods
        list(set=set, get=get, setInvMatrix = setInvMatrix, getInvMatrix=getInvMatrix)
}


## Description : The function first tries to get the inverse
##             : from the environment, in case the inverse is
##             : not available, it gets the inverse using
##             : solve method and sets it in the environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInvMatrix()
        print(inv)
        ##Check if the inv is available in cache or not
        if(!is.null(inv)){
                message("Getting the cached data")
                return(inv)
        }
        
        #If the cache inverse is not available then getting the matrix to calculate
        #inverse
        matr <- x$get()
        print(matr)
        #Calculating the inverse of the matrix using solve function
        inv <- solve(matr)
        print(inv)
        #Setting the inverse of the matrix in the cache
        x$setInvMatrix(inv)
        inv
}
