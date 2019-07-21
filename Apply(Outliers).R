#===============================================================================================
# INTRODUCING apply()
#===============================================================================================

#Consider the following matrix:
scores <- matrix(c(89,77,69,76,56,47,90,78,81,67,68,75), nrow = 4, ncol = 3)
rownames(scores) <- c("Amit", "Andy", "Abhishek", "Dibyo")
colnames(scores) <- c("Stat", "ML", "Prog")
scores


#Now let us address the following questions.
#1. What is the mean marks scored in each of the subjects?
#2. What is the mean marks scored by each of the students?
#3. What is the maximum maks scored in each subjects?
#4. What is the minimum marks scored by each of the students?

#Let us solve the first question...
#One way of solving it is definitely,

mean(scores[,1]) #average marks for stat
mean(scores[,2]) #average marks for ML
mean(scores[,3]) #average marks for Prog


#Note that in the above operations we actually implemented the mean() function in every
#possible columns in the matrix 'score' (But separately)



#============================================================================================
# COULD WE HAVE DONE IT ALL AT ONCE?
#============================================================================================

#Of course...Let us use the for loop

for(i in 1:ncol(scores))
{
  print(mean(scores[,i]))
}


#This is definitely a good alternate. But let us look for a better alternative.



#==================
# Using apply()
#==================

#apply() belongs to a family of functions called the loop function.
#apply() helps to implement a function in all the rows (defined by MARGIN=1) and all the 
#columns (defined by MARGIN=2) in an array, including a matrix.

#The general syntax for apply goes like this
#apply(X, MARGIN, FUN)
#where,
#X is an array, including a matrix
#MARGIN=1 represents rows and MARGIN=2 represents columns
#FUN the function to be applied

#Solution - Ques. 1
apply(scores, MARGIN = 1, FUN = mean) #will calculate the mean of all the rows of scores

#Similarly,
apply(scores, 2, mean)

#will calculate the mean of all the columns. Also note that it is not always 
#necessary to specify the parameters.

result = apply(scores, 2, mean)
class(result)
result['ML']


#===========================================================================================
# DEVELOPING AN APPLICATION
#===========================================================================================

#Let us create an application that will accept data as an argument and givr us that %age
#of missing values present in each variables of the data.

#STEP1: CREATING A SIMPLE FUNCTION
#Let us first create a function that will count the %age of missing values in a vector

pmiss <- function(x)
{
  miss <- sum(is.na(x))/length(x) *100
  return(miss)
}

#Test:
y <- c(NA,NA,NA,1)
pmiss(y)


#AN IMPROVISATION
#Can we skip storing the percentage in 'miss' in the above program and write in the following
#way?

setwd("C:\\Users\\Rohit\\Desktop\\ml sessions")
fram <- read.csv("framingham.csv")


pmiss <- function(x)
{
  return(sum(is.na(x))/length(x)*100)
}
  
#Test:
pmiss(x)



pmiss(fram$education)
pmiss(fram$glucose)
apply(fram, 2, FUN = pmiss)




#STEP2: USING apply() FUNCTION TO APPLY THIS FUNCTION TO ALL OUR COLUMNS IN THE DATA SET

library(MASS)
apply(Boston, 2, pmiss)

#The data Boston in the 'MASS' package has no missing values in it and therefore the above 
#program returns al zero. TRY t in the fram data set.

#TILL HERE WE ARE DONE BUILDING OUR BASIC STRUCTURES.



#STEP3: IMPROVIZATION
#Lets see what we can do with it...

#A.Storing the result in a vector
vec <- apply(Boston, 2, pmiss)
vec

#B.Convert it into a matrix to get a better print
as.matrix(vec)

#C.Let's store it
mat <- as.matrix(vec)

#D. Give the column name to the matrix
colnames(mat) <- "% Missing"
mat



#STEP4: CREATING THE FRAMEWORK
#Now let us put all these things together and design a function

pmissing <- function(data)
{
  #creating a local function
  pmiss <- function(x)
  {
    return(sum(is.na(x))/length(x)*100)
  }
  
  miss <- apply(data, 2, pmiss)
  miss <- as.matrix(miss)
  colnames(miss) <- "% Missing"
  
  return(miss)
}

pmissing(Boston)
pmissing(fram)


###...AND WE ARE DONE....SWEEEETTTTTTT!!!! :) ;)



##HOMEWORK
##1. Try to create the same function using the for loop.


##2. Create a function that can detect the presence of outliers in the variables of a dataset
##   The function will accept the data as an argument and return you a matrix indicating
##   which variable has outliers in it. The output is expected as follows:
##
##   OUTPUT:
##                Outlier
##   Variable1    -
##   Variable2    Outlier Detected
##   Variable3    Outlier Detected
##   Variable4    -
##   Variable5    Outlier Detected
##   
##   etc.


outliers0 <- function(data)
{
  #creating a local function
  detect <- function(x)
  {
    x=x[!is.na(x)] #remove na values from the given column 
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    ub=min(max(x),(q3+1.5*(q3-q1)))
    lb=max(min(x),(q3-1.5*(q3-q1)))
    flag=0
    for (i in x) #for each value of x check the condition
    {
      if (i<lb || i>ub) 
      {
        flag=1 #update the flag if condition is satisfied
        break
      }
  
    }
    {if(flag==1) #return as 'outlier detected' if the flag is updated
    {
      return('Outlier Detected') 
    }
    else
    {
    return('-')
    }}}
  
  
  
  detection <- apply(data, 2,detect) #using apply function on dataset columns
  detection <- as.matrix(detection)
  colnames(detection) <- "outlier"
  
  
  return(detection)
  
}


outliers0(fram)




##3. Create the function that will accept a data set as an argument and return the outlier
##   counts for each variables in the data set. The output is expected as follows
##
##   OUTPUT:
##                Upper Count     Lower Count
##   Variable1    0               0
##   Variable2    12              0
##   Variable3    1               21
##   Variable4    0               44
##   Variable5    0               1
##   
##   etc.

outliers1 <- function(data)
{
  #creating a local function
  ub <- function(x)
  {
    x=x[!is.na(x)]
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    res=min(max(x),(q3+1.5*(q3-q1)))
    count_ub=0 #to count values above upper boundary
    
    for (i in x) 
    {
      if(i>res) #if the value is greater than the upper boundary,we count it as outlier
      {
        count_ub=count_ub+1
      }}
    
    return(count_ub)
  }
  
  
  lb <- function(x) 
  {
    x=x[!is.na(x)]
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    res=max(min(x),(q3-1.5*(q3-q1)))
    
    count_lb=0 #to count values below lower boundary
    
    for (i in x) 
    {
      if(i<res)
      {
        count_lb=count_lb+1
      }}
    
    return(count_lb)
  }
  
  upperb <- apply(data, 2, ub)
  upperb <- as.matrix(upperb)
  lowerb <- apply(data, 2, lb)
  lowerb <- as.matrix(lowerb)
  outlier<-cbind(upperb,lowerb)
  colnames(outlier) <- c("uppercount","lowercount")
  return(outlier)
}

View(fram)

outliers1(fram[1:15])


##4. Add an extra feature to the function in 3. The function should return an object with
##   two extra columns that gives the count of the outliers when log transformation of the
##   variable is taken.


outliers <- function(data,k='default')
{
  #creating a local function
  ub <- function(x) 
  {
    x=x[!is.na(x)] #null values should not be present in the data
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    res=min(max(x),(q3+1.5*(q3-q1))) #k value of 1.5 is taken generally and depending on the requirement k can be changed
    count_ub=0 #To count values above upper boundary
    
    for (i in x) 
    {
      if(i>res) #if the value is greater than the upper boundary,we count it as outlier
      {
        count_ub=count_ub+1
      }}
    
    return(count_ub)
  }
  
  lb <- function(x)
  {
    x=x[!is.na(x)]#null values should not be present in the data
    q3=quantile(x,.75)
    q1=quantile(x,.25)
    res=max(min(x),(q3-1.5*(q3-q1)))
    count_lb=0 #to count values below lower boundary
    
    for (i in x) 
    {
      if(i<res)
      {
        count_lb=count_lb+1
      }}
    
    return(count_lb)
  }
  
  if (k=='default'|| k=='yes') #if k=default, only  columns with upper boundary (UB) and lower boundary (LB) are printed
                              #if ke=yes, in ad   dition to the UB and LB columns, counts post log trasformation are also generated   
  {
    upperb <- apply(data, 2, ub)
    upperb <- as.matrix(upperb)
    lowerb <- apply(data, 2, lb)
    lowerb <- as.matrix(lowerb)
    outlier<-cbind(upperb,lowerb)
    colnames(outlier) <- c("uppercount","lowercount")
  }
  
  #if counts with only log transformation are required then are required, then the bellow code will be executed 
  {if (k=='yes')
  {
    
    data1<-log(data+0.0001) # 0.0001 is a penalty added to handle o values
    upperb1 <- apply(data1, 2, ub)
    upperb1 <- as.matrix(upperb1)
    lowerb1 <- apply(data1, 2, lb)
    lowerb1 <- as.matrix(lowerb1)
    outlier1<-cbind(upperb1,lowerb1)#cbind appends columns in to the data frame
    colnames(outlier1) <- c("log_uppercount","log_lowercount")
    result<-cbind(outlier,outlier1)
    return(result)
  }
    else
    {
      return(outlier)
    }}
  
}


setwd("C:\\Users/sandi/Downloads")

fram <- read.csv("framingham.csv")

outliers(fram)
#outliers(fram,'default') #without log argument
outliers(fram,'yes')






  
  






















