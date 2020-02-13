#This program runs Bregman Iterations for Compressive Sensing. These methods are used in denoising images, especially in MRIs. This particular program will run
#a 3X5 system comprised of random values. The first procedure will complete a set number of iterations and the second/third will compute until certain tolerances
#are met within successive iterations of the function.

A <- matrix(c(runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), #produces a 3X5 matrix with values between -100 and 100
              runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100),
              runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100)), ncol=5,byrow =TRUE)
b <- matrix(c(runif(1, -100, 100), runif(1, -100, 100), runif(1, -100, 100)), ncol=1, byrow=TRUE) # produces a 3X1 Right Hand Side Vector with values between -100 and 100
lamda <- .005 #parameter used in the later function assignment
lamda
x0 <- matrix(c(0,0,0,0,0), ncol=1, byrow=TRUE) #Initialize a guess for the solution to the system
b0 <- matrix(c(0,0,0), ncol=1, byrow=TRUE)
x0
b0
flag =1 #Boolean variable
flag2=1
count =0 #Iteration counters
count2=0
count3=0

for (k in 1:1000){ #completes the Bregman Iteration a fixed number of times, here 1000
  b1 <- matrix(c(b[1,1]+b0[1,1]-A[1,1]*x0[1,1]-A[1,2]*x0[2,1]-A[1,3]*x0[3,1]-A[1,4]*x0[4,1]-A[1,5]*x0[5,1], 
                 b[2,1]+b0[2,1]-A[2,1]*x0[1,1]-A[2,2]*x0[2,1]-A[2,3]*x0[3,1]-A[2,4]*x0[4,1]-A[2,5]*x0[5,1],
                 b[3,1]+b0[3,1]-A[3,1]*x0[1,1]-A[3,2]*x0[2,1]-A[3,3]*x0[3,1]-A[3,4]*x0[4,1]-A[3,5]*x0[5,1]), ncol=, byrow=TRUE)
  
  x1 <- function(x) .5*((A[1,1]*x[1]+A[1,2]*x[2]+A[1,3]*x[3]+A[1,4]*x[4]+A[1,5]*x[5]-b1[1,1])^2 +
                          (A[2,1]*x[1]+A[2,2]*x[2]+A[2,3]*x[3]+A[2,4]*x[4]+A[2,5]*x[5]-b1[2,1])^2 +
                          (A[3,1]*x[1]+A[3,2]*x[2]+A[3,3]*x[3]+A[3,4]*x[4]+A[3,5]*x[5]-b1[3,1])^2)+
    lamda*(abs(x[1])+abs(x[2])+abs(x[3])+abs(x[4])+abs(x[5])) #test function
  x=optim(c(0,0,0,0,0), x1)$par #minimizes f with x>=-100, y>=-100 and returns x[1], ..., x[5] that min f
  y=optim(c(0,0,0,0,0), x1)$value #gives min function value in interval
  
  
  for (i in 1:nrow(b1) ){ # Uses b0 as a holder for the values in b1
    for (j in 1:ncol(b1)){
      b0[i,j]<-b1[i,j]
    }
  }
  
  for (k in 1:length(x0)){# Uses x0 as a holder for the optimal values for f
    x0[k,1]=x[k]
    
  }
  
  count=count+1 # counts the number of iterations 
}
Estx1<-x #Assigns Estx1 as a permanent holder of the values of x
Est1b <-b0 #Assigns Est1b as a permanent holder of the values of b0
for (i in 1:nrow(b0)){ #Resets all values of b0
  for (j in 1:ncol(b0)){
    b0[i,j]=0 
  }
}

while(flag==1){ #dials in on a specific solution
  flag=0
  
  
  b1 <- matrix(c(b[1,1]+b0[1,1]-A[1,1]*x0[1,1]-A[1,2]*x0[2,1]-A[1,3]*x0[3,1]-A[1,4]*x0[4,1]-A[1,5]*x0[5,1],
                 b[2,1]+b0[2,1]-A[2,1]*x0[1,1]-A[2,2]*x0[2,1]-A[2,3]*x0[3,1]-A[2,4]*x0[4,1]-A[2,5]*x0[5,1],
                 b[3,1]+b0[3,1]-A[3,1]*x0[1,1]-A[3,2]*x0[2,1]-A[3,3]*x0[3,1]-A[3,4]*x0[4,1]-A[3,5]*x0[5,1]), ncol=, byrow=TRUE)
  
  x1 <- function(x) .5*((A[1,1]*x[1]+A[1,2]*x[2]+A[1,3]*x[3]+A[1,4]*x[4]+A[1,5]*x[5]-b1[1,1])^2 +
                          (A[2,1]*x[1]+A[2,2]*x[2]+A[2,3]*x[3]+A[2,4]*x[4]+A[2,5]*x[5]-b1[2,1])^2 +
                          (A[3,1]*x[1]+A[3,2]*x[2]+A[3,3]*x[3]+A[3,4]*x[4]+A[3,5]*x[5]-b1[3,1])^2)+
    lamda*(abs(x[1])+abs(x[2])+abs(x[3])+abs(x[4])+abs(x[5])) #test function
  x=optim(c(0,0,0,0,0), x1)$par #minimizes f with x>=-100, y>=-100 and returns x[1], ..., x[5] that min f
  y=optim(c(0,0,0,0,0), x1)$value #gives min function value in interval}
  
  
  for (i in 1:nrow(b1) ){ # Uses b0 as a holder for the values in b1
    for (j in 1:ncol(b1)){
      b0[i,j]<-b1[i,j]
    }
  }
  for (dim in 1:length(x0)){ #Checks for the values of each entry in x to be within 10 e-05 of the previous iteration
    if (abs(x[dim]-x0[dim,1])>10e-05){
      flag=1
    }
  }
  
  for (k in 1:length(x0)){ # Uses x0 as a holder for the optimal values for f
    x0[k,1]=x[k]
    
  }
  count2=count2+1 #Counts number of iterations for this procedure
}
Estx2 <-x #Assigns Estx2 as a permanent holder of the values of x
Est2b <-b0 #Assigns Est2b as a permanent holder of the values of b0

for (i in 1:nrow(b0)){ #Resets all values of b0
  for (j in 1:ncol(b0)){
    b0[i,j]=0 
  }
}


while(flag2==1){ #dials in on a tolerance between actual b and estimate of b using approximate minimum
  flag2=0
  
  b1 <- matrix(c(b[1,1]+b0[1,1]-A[1,1]*x0[1,1]-A[1,2]*x0[2,1]-A[1,3]*x0[3,1]-A[1,4]*x0[4,1]-A[1,5]*x0[5,1],
                 b[2,1]+b0[2,1]-A[2,1]*x0[1,1]-A[2,2]*x0[2,1]-A[2,3]*x0[3,1]-A[2,4]*x0[4,1]-A[2,5]*x0[5,1],
                 b[3,1]+b0[3,1]-A[3,1]*x0[1,1]-A[3,2]*x0[2,1]-A[3,3]*x0[3,1]-A[3,4]*x0[4,1]-A[3,5]*x0[5,1]), ncol=, byrow=TRUE)
  
  x1 <- function(x) .5*((A[1,1]*x[1]+A[1,2]*x[2]+A[1,3]*x[3]+A[1,4]*x[4]+A[1,5]*x[5]-b1[1,1])^2 +
                          (A[2,1]*x[1]+A[2,2]*x[2]+A[2,3]*x[3]+A[2,4]*x[4]+A[2,5]*x[5]-b1[2,1])^2 +
                          (A[3,1]*x[1]+A[3,2]*x[2]+A[3,3]*x[3]+A[3,4]*x[4]+A[3,5]*x[5]-b1[3,1])^2)+
    lamda*(abs(x[1])+abs(x[2])+abs(x[3])+abs(x[4])+abs(x[5])) #test function
  x=optim(c(0,0,0,0,0), x1)$par #minimizes f with x>=-100, y>=-100 and returns x[1], ..., x[5] that min f
  y=optim(c(0,0,0,0,0), x1)$value #gives min function value in interval}
  
  
  
  
  for (i in 1:nrow(b1) ){ # Uses b0 as a holder for the values in b1
    for (j in 1:ncol(b1)){
      b0[i,j]<-b1[i,j]
    }
  }
  for (dim in 1:length(b0)){ #Checks for the values of b to be within 10e-04 of the previous iteration
    if (abs(b[dim,1]-b0[dim,1])>10e-04){
      flag2=1
    }
  }
  
  for (k in 1:length(x0)){ # Uses x0 as a holder for the optimal values for f
    x0[k,1]=x[k]
    
  }
  count3=count3+1 #Counts the number of iterations for this procedure
}
Estx3 <-x #Assigns Estx3 as a permanent holder of the values of x
Est3b <-b0 #Assigns Est3b as a permanent holder of the values of b0

Estx1 #Displays all values calculated through the 3 procedures
Est1b
count
Estx2
Est2b
count2
Estx3
Est3b
count3

