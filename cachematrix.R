## cachematrix.R
## Functions: makeCacheMatrix, cacheSolve
## Belows are the functions that cache the inverse of a matrix
## makeCacheMatrix is a function which makes space to save cached inverse matrix
## when cacheSolve function is called, it check whether the variable m in makeCacheMatrix is null or not.
## if it is null, it calculates the inverse matrix and saves it m using setinverse
## if it is not null, which means variable m already has cached inverse matrix,
## so return m


## makeCacheMatrix contains cached inverse matrix and returns list of set, get, setinverse, getinverse

makeCacheMatrix <- function(x = numeric()) { #i.e. x <- matrix(c(2,4,3,1,5,7), nrow=2, ncol=3, byrow = TRUE)
    m <- NULL            #at first, m is set as NULL
    set <- function(y) { #set function 
        x <<- y          #set x the matrix to be inversed (y is assigned to the variable x which is in the parent envrionment)
        m <<- NULL       #saves NULL value to a variable m, which locates in parent environment
    }
    get <- function() x                       #get function returns x, the original input matrix
    setinverse <- function(solve) m <<- solve #setinverse using solve function
    getinverse <- function() m                #getinverse return the value inside of var m
    list(set = set, get = get,                #return the functions inside of list
         setinverse = setinverse,
         getinverse = getinverse)
}

##cacheSolve function returns a matrix that is the inverse of 'x' 
##It calls cached inverse function or if there is no cached inverse matrix, calculate one and cache it

cacheSolve <- function(x, ...) {  ##i.e. cachemean(makeCacheMatrix(x))
    m <- x$getinverse()                #call m value from input matrix x; null at first but after that, it contains the cached inverse matrix
    if(!is.null(m)) {                  #check m is null or not and if not null just return cached inverse matrix m
        message("getting cached data") #indicates that the return value is cached value
        return(m)
    }
    data <- x$get()        #if m is null then get x value using get()
    m <- solve(data, ...)  #get the inverse of matrix m
    x$setinverse(m)        #save the inverse matrix by using setinverse function
    m                      #return m
}