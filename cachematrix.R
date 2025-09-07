## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function takes a matrix as an input
## The set function assigns values to the x variable (to the eclosing env)
## The get fucntion retrieves the stored value of x
## The setInv function contains the inverse value of x coming from cacheSolve funnction.
## The getInv function fetches the inverse value of x.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setInv <- function(inverse) i<<- inverse
  getInv <- function() i
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}




## The cacheSolve function returns the already stored inverse matrix if x not updated
## If i doesnot have a value then,
## 1) The value of x will be fethed from the instance of makeCacheMatrix function
## 2) The inverse matrix of x will be calculated and stored in i
## 3) The i value will be stored to the instance and can be retrived-
##    -using the getInv function.
## Note: the cacheSolve fucntion expects a squaure matrix to compute the-
##       -inverse otherwise, it will return an error.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)){
    print("getting cached data")
    return(i)
  }
  data<-x$get()
  i <- solve(data)
  x$setInv(i)
  i
  
}
