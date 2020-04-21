## The functions basically make a list which store the data of inverse of a matrix 
## in cache memory so that the computation doesn't need to happen everytime a matrix 
## inverse is needed
##  
## Creation of a list which stores the value of the matrices and their inverse

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    get <- function(){
        x
    }
    
    setinverse <- function(inv){
        im <<- inv
    }
    
    getinverse <- function(){
        im
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculation of inverse, storing of that inverse in the list and printing the result

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    
    if(!is.null(im)){
        message("Inverse Present. Getting Matrix.")
        return(im)
    }
    else{
        im <- solve(x$get())
        
        x$setinverse(im)
        
        im
    }
}