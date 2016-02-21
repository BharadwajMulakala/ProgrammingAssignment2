## Our aim is to Cache the inverse of a matrix.

## makeCacheMatrix is function which takes a matrix as an argument
makeCacheMatrix <- function(x = matrix())
  {
## This getFun() gets the argument of parent function (i.e in this case a matrix x)
  getFun <- function() 
  {
        return(x);
  }
  setFun <- function(y)
  {
        x<<-y;
        inv <<- NULL;
  }
        inv <- NULL;#initializing the inverse (matrix) as NULL
  getInvFun <- function()
  {
        return(inv);
  }
  setInvFun <- function(inverse)
  {
        inv <<- inverse;
  }
 list(setFun = setFun, getFun =getFun, setInvFun =setInvFun, getInvFun =getInvFun)
}
## This cacheSolve function creates the inverse of the matrix created by makeCacheMatrix.
## And it checks whether the iverse has already been calculated, if so then it simply returns the cache.
cacheSolve <- function(x, ...)
{
  inv <- x$getInvFun();
  if(!is.null(inv))
  {
        message("Now this is from the cache...")
        return(inv);
  }
  matri <- x$getFun();
  inv <- solve(matri, ...);##Solving the matrix for it's inverse using inbuilt solve function
  x$setInvFun(inv);
  return(inv); ## Returning a matrix that is the inverse of 'x'
}
