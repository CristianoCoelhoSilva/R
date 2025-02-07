data("macrodata")
dep<-macrodata[,"unrate",drop=FALSE]
ind<-macrodata[,-1,drop=FALSE]

train.end<-"2008-12-01"

autoML <- tts.autoML(y=dep, x=ind, train.end,arOrder=c(2,4), xregOrder=c(0,1,3),type="both")
print(autoML$modelsUsed,n=22) #View the AutoML Leaderboard
#testData2 <- window(autoML$dataused,start="2009-01-01",end=end(autoML$dataused))

#P1<-iForecast(Model=autoML,Type="static",newdata=testData2)
#P2<-iForecast(Model=autoML,Type="dynamic",a.head=nrow(testData2))

#tail(cbind(testData2[,1],P1))
#tail(cbind(testData2[,1],P2))