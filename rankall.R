rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        colnames(data)[11] <- "heartattack"
        colnames(data)[17] <- "heartfailure"
        colnames(data)[23] <- "pneumonia"
        # numeric conversion
        data[,11] <- as.numeric(data[,11])
        data[,17] <- as.numeric(data[,17])
        data[,23] <- as.numeric(data[,23])
        
        ## Check that state and outcome are valid
        selOutcome <- match(outcome,c("heart attack", "heart failure", "pneumonia"))
        selOutcome <- c(11,17,23)[selOutcome]
        if (is.na(selOutcome)) stop('Invalid outcomeout. Must be one of "heart attack", "heart failure", "pneumonia"')
        
        ##Return a data frame with the hospital names and the state
        ##split data by state and apply order to each
        data <- split(data[,c(2,7,selOutcome)],data$State)
        if (num=="best") num <- 1
        if (num=="worst") {
                data<-lapply(data,function(x) x[order(-x[,3],x[,1]),1][1])
        }
        else {
                data<-lapply(data,function(x) x[order(x[,3],x[,1]),1][num])
        }
        
        ##Convert the list to data frame
        data<-as.data.frame(data)
        data<-cbind(t(data),colnames(data))
        data<-as.data.frame(data)
        colnames(data)<-c("hospital","state")
        data
}