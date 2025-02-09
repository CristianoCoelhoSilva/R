library(zoo)

#head(macrodata)
#class(macrodata)
#is.data.frame(macrodata)

#Case 1. Low frequency
data("macrodata")
dep <- macrodata[569:669,"unrate",drop=FALSE]
ind <- macrodata[569:669,-1,drop=FALSE]

#df <- data.frame(Date = index(macrodata), Value = coredata(macrodata))
#dfdep <- data.frame(Date = index(dep), Value = coredata(dep))
#dfind  <- data.frame(Date = index(ind), Value = coredata(ind))

train.end <- "2018-12-01"# Choosing the end dating of train
models <- c("glm","knn","nnet","rpart","rf","svm","enet","gbm","lasso","bridge","nb")[2]
type <- c("none","trend","season","both")[1]

output <- tts.caret(y=dep,x=NULL, arOrder=c(1), xregOrder=c(1),
                    method=models, tuneLength =1, train.end, type=type,
                    resampling=c("boot","cv","repeatedcv")[1],preProcess = "center")


testData1 <- window(output$dataused,start="2019-01-01",end=end(output$dataused))
P1 <- iForecast(Model=output,Type="static",newdata=testData1)
P2 <- iForecast(Model=output,Type="dynamic",a.head=nrow(testData1))
tail(cbind(testData1[,1],P1,P2))



#Case 2. High frequency
head(ES_15m)
head(ES_Daily)
dep <- ES_15m #SP500 15-minute realized absolute variance
ind <- NULL
train.end <- as.character(rownames(dep))[as.integer(nrow(dep)*0.9)]
models<-c("svm","rf","rpart","gamboost","BstLm","bstSm","blackboost")[1]
type<-c("none","trend","season","both")[1]
 output <- tts.caret(y=dep, x=ind, arOrder=c(3,5), xregOrder=c(0,2,4),
 method=models, tuneLength =10, train.end, type=type,
 resampling=c("boot","cv","repeatedcv")[2],preProcess = "center")
testData1<-window(output$dataused,start="2009-01-01",end=end(output$dataused))
P1<-iForecast(Model=output,Type="static",newdata=testData1)
P2<-iForecast(Model=output,Type="dynamic",a.head=nrow(testData1))
tail(cbind(testData1[,1],P1,P2))

