x = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE) 
y = matrix(1:20, nrow = 5, ncol = 4, byrow = FALSE) # default is "false" (by colomn)

x[2,] #get the 2nd row
x[,2] #get the 2nd colomn
x[1,4]

x[2,c(2,4)] # in the second row, the 2nd, 3rd and 4th value

x[2:5,2] # in the second colomn, the 2nd, 3rd, 4th and 5th value

rnames=c("apple","pear","berry","banana","pineapple")
cnames=c("dog", "cat", "chicken","duck")

rownames(x)=rnames 
colnames(x)=cnames #give the row and colomn names

# create an array
dim1=c("a1","a2")
dim2=c("b1","b2","b3")
dim3=c("c1","c2","c3","c4")
dim4=c("d1","d2","d3")
z=array(1:72,c(2,3,4,3),dimnames = list(dim1,dim2,dim3,dim4)) # 2*3*4*3=72
View(z)

z[1,2,4,3] #for a complicated 4-dimension array, it can only be listed as tables, so use "view" to see its content
# [1,2,4,3] means the 1st, 2nd, 4th and 3rd in every dimension!


#data frame - big matrix!
patientID <- c(1:4)
age <- c(25:28)
diabetes  <- c("type1","type2","type3","type4")
status <- c("poor","good","excellent","died")
patientdata <- data.frame(patientID,age,diabetes,status)
patientdata[1:2] #default is [,colomn]
patientdata[1:2,]
patientdata[1,1:3]
patientdata[1:3,1:3] #from the 1st to the 3rd row
patientdata[c(1,3),1:3] # the 1st and 3rd row(in this case, it is necessary to use c())

#list can eat anything!
mylist <- list(age,z,dim1)
View(mylist)
mylist[2]
mylist[[2]][1,1,1,3] #choose the 2nd dimension of the list first, and then choose a1,b1,c1 and d3.


par(mfrow=c(2,2)) #we can create 4 plots at same time without pictures being replaced!
plot(rnorm(50),pch=8) #pch is for the shape of scatters
plot(rnorm(28),type="l",lty=5) # typle l is for lines, and lty is line type.
plot(rnorm(30),cex=1) #cex is for the size of dots.
plot(rnorm(200),lwd=2) #lwd is for line width

par(mfrow=c(1,1))
plot(rnorm(50),pch=8)
title(main = "normal dist")     
     