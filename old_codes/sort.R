dataset <- read.csv("test.csv",stringsAsFactors = FALSE)
names(dataset)
View(dataset)
sortFrame(dataset,word,alphabet=TRUE)


txt <- c("bob","Clare","clare","bob","eve","eve")
num1 <- c(3,1,2,0,0,2)
num2 <- c(1,1,3,0,3,2)
etc <- c("not","used","as","a","sort","term")
dataset <- data.frame( txt, num1, num2, etc, stringsAsFactors=FALSE )
sortFrame( dataset, txt )