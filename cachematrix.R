## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## First we make a function makeCacheMatrix which provides us with the cached value
## In this function we define 4 other functions which works as follows:-
## 1)set: sets the value of the matrix
## 2)get: gets the value of the matrix
## 3)setinverse: sets the inverse value of the matrix
## 4)getinverse: gets the inverse value of the matrix

makeCacheMatrix <- function(x){
        inv <- NULL
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv <<- inverse
        getinverse<-function() inv 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        ## here we have created a list of the functions with the name set, get, setinverse and getinverse respectively.
        ## This is really an important part otherwise while performing for e.g x$get() you will get an error.
}


## Here is the second function we have created here namely cacheSolve.
## This function takes the output from makeCacheMatrix function as an input.
## It fist checks whether is there any cached value provided by the makeCachedMatrix function or not.
## If no cached value is provided then it computes the inverse of the given matrix.
## If the function provides a cached value then it prints a message " getting cached value" and actually doesnt undergoes any further processing for solving the inverse.
## It is assumed that the matrix provided here is invertible.

cacheSolve <- function(x,...){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached value")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        inv
}
