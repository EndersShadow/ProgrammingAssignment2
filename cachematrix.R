## The following two functions (makeCacheMatrix and CacheSolve) will conserve
## memory when matrix inversion is needed. These functions will allow the 
## inverted matrix to be cached so it can be used over and over again.

## makeCacheMatrix will set and get the value of a matrix as well as set and get
## the value of the inverse for that matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix<-NULL
        set <- function(y) {
                x<<-y
                inv_matrix<<-NULL
        }
        get <- function() x
        setinverse <-function(inverse) inv_matrix<<-inverse
        getinverse<-function() inv_matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Given that the matrix is invertable the cacheSolve function will compute
## the inverse of the matrix and store it in cache to be used again if the matrix
## is called again. 

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {
                message("getting cached data.")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data)
        x$setinverse(inv_matrix)
        inv_matrix
}
