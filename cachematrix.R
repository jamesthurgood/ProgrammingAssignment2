## The purpose of these functions is to cache and make use of a cache 
## of the inverse of a matrix as this is potentially an expensive
## process.

## makeCacheMatrix has four setter and getter functions:
## 1 - set: set the matrix to the value of the x parameter
## 2 - get: the matrix which was set
## 3 - setTheInverse: set the inverse value
## 4 - getTheInverse: get the inverse value

## usage:
##  create a square matrix
##      x <- matrix(rnorm(9), nrow = 3) 
##  parse that to our function
##      z <- makeCacheMatrix(x)
##  solve it 
##      cacheSolve(z)
##  and then solve it again to see the same result 
##  preceded by the message "getting cached data"
makeCacheMatrix <- function(x = matrix()) {
        #theInverse - an object to store the inverse of the matrix
        theInverse <- NULL
        
        #Getters & setters
        set <- function(y) {
                x <<- y
                theInverse <<- NULL
        }
        get <- function() x
        setTheInverse <- function(inverse) theInverse <<- inverse
        getTheInverse <- function() theInverse
        
        #list the functions
        list(
             set = set, 
             get = get,
             setTheInverse = setTheInverse,
             getTheInverse = getTheInverse
            )
}


## cacheSolve: calculate the inverse of a matrix and store in a cache
## using the functions in makeCacheMatrix and then return the inverse

cacheSolve <- function(x, ...) {
        ## get the inverse
        theInverse <- x$getTheInverse()
        
        ## does the inverse contain data?
        if(!is.null(theInverse)) {
                # if so don't do anything! drop through to the end 
                # and return the cached inverse
                message("getting cached data")
        } else {
                #else, solve it and cache it and return below
                data <- x$get()
                theInverse <- solve(data, ...)
                x$setTheInverse(theInverse)
        }
        return(theInverse)
}

## unit test results
#> source("cachematrix.R")
#> x <- matrix(rnorm(9), nrow = 3) 
#> z <- makeCacheMatrix(x)
#> cacheSolve(z)
#[,1]       [,2]        [,3]
#[1,]  0.007385685 -0.3235076  0.86746847
#[2,] -0.265964537  0.5521529 -0.08879565
#[3,] -0.278867779 -0.2137555 -0.25918585
#> cacheSolve(z)
#getting cached data
#[,1]       [,2]        [,3]
#[1,]  0.007385685 -0.3235076  0.86746847
#[2,] -0.265964537  0.5521529 -0.08879565
#[3,] -0.278867779 -0.2137555 -0.25918585