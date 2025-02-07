# Cross-validation takes time, example below is commented.
## Machine Learning by library(caret)
#Case 1. Low frequency, regression type
data("macrodata")
dep <- macrodata[569:669,"unrate",drop=FALSE]
ind <- macrodata[569:669,-1,drop=FALSE]
train.end <- "2018-12-01"# Choosing the end dating of train

models <- c("svm","rf","rpart","gbm","nb")[1]
type <- c("none","trend","season","both")[1]

#output <- tts.caret(y=dep, x=ind, arOrder=c(1), xregOrder=c(1),
#method="svm", tuneLength =1,
#train.end, type=type,resampling="cv",preProcess = "center")

#testData1 <- window(output$dataused,start="2019-01-01",end=end(output$dataused))
#P1=iForecast(Model=output,Type="static",newdata=testData1)
#P2=iForecast(Model=output,Type="dynamic",a.head=7)
#tail(cbind(testData1[,1],P1))
#tail(cbind(testData1[,1],P2))

#Case 2. Low frequency, binary type
data(bc) #binary dependent variable, business cycle phases
dep=bc[,1,drop=FALSE]
ind=bc[,-1]

train.end=as.character(rownames(dep))[as.integer(nrow(dep)*0.8)]
test.start=as.character(rownames(dep))[as.integer(nrow(dep)*0.8)+1]
output = tts.caret(y=dep, x=ind, arOrder=c(1), xregOrder=c(1), method="nb",
 tuneLength =10, train.end, type=type)

testData1=window(output$dataused,start=test.start,end=end(output$dataused))

head(output$dataused)
P1=iForecast(Model=output,Type="static",newdata=testData1)
P2=iForecast(Model=output,Type="dynamic",a.head=7)
tail(cbind(testData1[,1],P1),10)
tail(cbind(testData1[,1],P2),10)
