#Excuse me for typos and do comment on the assignment, THANKYOU
#The first function takes a matrix as an argument and cached it into memory
#the second function finds the inverse of the matrix supplied to the first function
#brings it into the memory return it, keeps it there unless an new matrix is supplied

# makeCacheMatrix takes a matrix say m1 as a argument, then it initilizes "inv" as NULL,
#the newmat function changes the value on the address of m1 with m2
#and initilizes "inv" as NULL again to clear any previous value
#getmat takes the matrix 
#newinv changes the value of inv with the supplied argument 
#getinv takes the value of inv

makeCacheMatrix <- function(m1 = matrix()) {
        inv <- NULL
        newmat <- function(m2){
                m1 <<- m2
                inv <<- NULL
        }
        getmat <- function(){
                m1
        }
        newinv <- function(inverse) {
                inv <<- inverse
        }
        getinv <-function(){
                inv
        }
        list(newmat = newmat,getmat = getmat, newinv = newinv,getinv = getinv)
}                                        


#cacheSolve, checks if the inv variable has a NULL value or not, if no then,
#it prints the current value.If yes,then it gets a new value from the makeCacheMatrix's getmat function
#solves it for the inverse, stores it in the newinv variable of the former function and returns it 

cacheSolve <- function(m1, ...) {
        inv <- m1$getinv()
        if(!is.null(inv)){
                print("checking memory")
                return(inv)
                print(inv)
        }
        temp <- m1$getmat()
        inv <- solve(temp)
        m1$newinv(inv)
        inv
}
