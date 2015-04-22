## Put comments here that give an overall description of what your
## functions do

## Function in which we create 3 other functions - 1. get: to get the data of the matrix (used in
## case the vector does not exist yet), 2. setinverse: function which will allow us to assign the
## inverse to a variabele, 3. getinverse: after a value is assign to the variabele, we can use the
## getinverse to see if the inverse was already calculated before

makeCacheMatrix <- function(x = matrix()) {
        Minverse <- NULL ##put inverse to NULL (Minverse = Matrix Inverse)
        get<-function()x ##create function to get value of x (=matrix)
        setinverse<-function(solve) Minverse <<- solve ## function to set inverse
        getinverse<-function() Minverse ## function to get inverse back
        list(get = get, setinverse = setinverse, getinverse = getinverse)
}

## function use to check via functions created above if the inverse exists, if yes show the value
## if no calculate the inverse, assign it to the variabel and show it.

cacheSolve <- function(x, ...) {
        Minverse <- x$getinverse()   ##use function created above to getinverse and assign it to var
        if(!is.null(Minverse)){     ## if inverse is not NULL, return value
                message("getting cached data")
                return(Minverse)
        }
        data <- x$get() ## if inverse is null, new vector, use get function to extract data
        Minverse <- solve(data) ## use solve function on data to get inverse
        x$setinverse(Minverse) ## use set function to assign inverse to value
        Minverse        ## show result
}

## Return a matrix that is the inverse of 'x'
