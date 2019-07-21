setwd("/Users/anaghakaranam/Desktop/Praxis_class/Trimester2/Intro2R/Assignment/Assignment/")

library("BaylorEdPsych")
library("mice")
library("VIM")
library("miceMNAR")

#mcar - missing because of chance
#mar - missing because of chance in subgroups
#note: for simplicity, we assume mcar & mar as the same.
#mnar - they are missing because of a reason and we need to check the same


threshold_test <- function(dataset, threshold = 0.05){
  
  percen_values = sapply(dataset, function(x) sum(is.na(x))/length(x)*100 ) 
  print("% of NA values are:")
  print(percen_values)
  columns_test = colnames(dataset)[percen_values < threshold]
  
  new_dataset = dataset[columns_test]

  return(new_dataset)
}

pvalue_test <- function(dataset,significance){
  
  mcar = LittleMCAR(dataset)
  print("pvalue is:")
  print(mcar)
  return(mcar$p.value) 
}


treat_missing <- function(dataset, threshold = 0.05, significance = 0.05, method_mice = "pmm"){
  
  
  dataset = threshold_test(dataset,threshold)
  
  
  
  #We perform MACR-MNR test for these new_dataset
  p_value = pvalue_test(dataset,significance)
  print("pvalue is:")
  p_value
  print("Is pvalue < significance?")
  print(p_value<significance)
  
  if(p_value > significance){
    #mnar test
    
    arg <- MNARargument(data=simul_data,varMNAR="Y",JointModelEq=JointModelEq)
    
    imputation2 <- mice(data = arg$data_mod,
                        method = arg$method,
                        predictorMatrix = arg$predictorMatrix,
                        JointModelEq=arg$JointModelEq,
                        control=arg$control,
                        maxit=1,m=5)
    analysis2 <- with(imputation2,lm(Y~X1+X2+X3))
    result2 <- pool(analysis2)
    return(result2)
  }
  else{
    
    #mcr/mcar test
    tempData <- mice(dataset,m=5,maxit=50,meth=method_mice,seed=500)
    summary(tempData)
    modelFit1 <- with(tempData,lm(Temp~ Month+Wind))
    result <- pool(modelFit1)
    return(result)
  }
  
  
}


data("airquality")

output = treat_missing(dataset = airquality,threshold = 0.05,
                       significance = 0.05,method_mice = "pmm")
output



#we return no_imputations number of imputed datasets



#if data was missing randomly, we get 5 imputed datasets.
#we can choose the best one based on our interpretation or use pool command
#when fitting a linear regression model to get consolidated results.


#MNAR. 
#generate MNAR data by simulating for demonstration purposes...
X1 <- rnorm(500,0,1)
X2 <- rbinom(500,1,0.5)
X3 <- rnorm(500,1,0.5)
errors <- rmvnorm(500,mean=c(0,0),sigma=matrix(c(1,0.3,0.3,1),nrow=2,byrow=TRUE))
Y <- X1+X2+errors[,1]
Ry <- ifelse(0.66+1*X1-0.5*X2+X3+errors[,2]>0,1,0)
Y[Ry==0] <- NA
simul_data <- data.frame(Y,X1,X2,X3)
JointModelEq <- generate_JointModelEq(data=simul_data,varMNAR = "Y")
JointModelEq[,"Y_var_sel"] <- c(0,1,1,1)
JointModelEq[,"Y_var_out"] <- c(0,1,1,0)



result = treat_missing(simul_data, threshold=0.05, significance=0.05)
print(result)

