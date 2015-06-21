#                ABOUT cachematrix.R
#cachematrix.R has two functions (makeCacheMatrix, cacheSolve) that
#work together in order to determine if the user inputted information 
#has been cached by makeCacheMarix or if the matrix inverse needs to 
#be processed within cacheSolve.

#                ABOUT makeCacheMatrix
#the purpose of this function is to store information 
#about the matrix_inverse in a list so that it may be
#later called upon by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        #matrix_inverse is set to null
        matrix_inverse <- NULL
        
        #set is assigned the value of function(y)
        set <- function(y) {
                #x is equal to object value of y
                x <<- y
                #matrix_inverse is given the object value of null
                matrix_inverse <<- NULL
        }
        
        ##get is assigned the value of function() x
        get <- function() x
        
        ##setmatrix_inverse stores function(inverse) matrix inverse
        #which in term is assigned inverse as object value
        setmatrix_inverse<- function(inverse) matrix_inverse <<-inverse
        
        ##getmatrix_inverse stores function() matrix _inverse
        getmatrix_inverse <- function() matrix_inverse
        
        ##list stores set, get, setmatrix_inverse, getmatrix_inverse
        list(set = set, get = get,
             setmatrix_inverse = setmatrix_inverse,
             getmatrix_inverse = getmatrix_inverse)
}

#               ABOUT cacheSolve
#If matrix_inverse has been calculated a message will be outputted 
#to the user and the function will return the matrix_inverse already
#calculated. If matrix_inverse is equal to null cacheSolve
#cacheSolve will run through the proper procedure to calculate the 
#matrix_inverse and return the new matrix_inverse

cacheSolve <- function(x, ...) {
        #the matrix_inverse is equal to x$getmatrix_inverse()
        matrix_inverse <- x$getmatrix_inverse()
        
        #if matrix_inverse is not equal to is.null
        if (!is.null(matrix_inverse)) {
       
                ##OUPUT TO USER
                message("The cached data is being processed")
                
                ##returns the resulting value of the matrix_inverse
                return(matrix_inverse)
        } else {
                ##User input is used to get the inverse 
                ##matrix using the solve function
                matrix_inverse <- solve(x$get())
                
                ##matrix_inverse is processed through the 
                ##setmatrix_inverse function
                x$setmatrix_inverse(matrix_inverse)
                
                ##returns the resulting value of the matrix_inverse
                return(matrix_inverse)
        }
}

#Assignment Source: https://github.com/rdpeng/ProgrammingAssignment2