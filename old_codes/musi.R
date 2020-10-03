dataset <- read.csv("testnepalidata.csv",stringsAsFactors = FALSE)
newdata <- dataset[c(-1,-3,-4)]
View(newdata)

data1=newdata[which(newdata$sentiment=='प्रेम'),]
View(data1)

data2=newdata[which(newdata$sentiment=='दुखी'),]

data3=newdata[which(newdata$sentiment=='देश्भक्ती'),]

data4=newdata[which(newdata$sentiment=='आशा'),]

data5=newdata[which(newdata$sentiment=='खुशी'),]

data6=newdata[which(newdata$sentiment=='सकारात्मक'),]

data7=newdata[which(newdata$sentiment=='विश्वास'),]

data8=newdata[which(newdata$sentiment=='डर'),]

data9=newdata[which(newdata$sentiment=='रिस'),]

data10=newdata[which(newdata$sentiment=='नकारात्मक'),]

data11=newdata[which(newdata$sentiment=='घृणा'),]

data12=newdata[which(newdata$sentiment=='आश्चर्य'),]

colnames(data1)<- c("प्रेम")
colnames(data2)<- c("दुख")
colnames(data3)<- c("देश्भक्ती ")
colnames(data4)<- c("आशा")
colnames(data5)<- c("खुशी")
colnames(data6)<- c("सकारात्मक")
colnames(data7)<- c("विश्वास")
colnames(data8)<- c("डर")
colnames(data9)<- c("रिस")
colnames(data10)<- c("नकारात्मक")
colnames(data11)<- c("घृणा")
colnames(data12)<- c("आश्चर्य")
