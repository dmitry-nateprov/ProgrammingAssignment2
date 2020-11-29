## We have 2 functions
## makeCacheMatrix consists of set, get, set_inv, get_inv

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL                                 #initializing inverse as Null
    set <- function(y){
        x <<- y
        inv <- NULL
    }
    get <- function() x                         #get matrix x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## This is used to get the cache data

cacheSolve <- function(x, ...) {                #gets cache data
    inv <- x$get_inv()
    if(!is.null(inv)){                          #checks inverse is Null or not
        message("getting cashed result")
        return(inv)                             #returns inverse value
    }
    dat <- x$get()
    inv <- solve(dat, ...)                      #calculates inverse value
    x$set_inv(inv)
    inv                                         #returns inversed "x" matrix
}



