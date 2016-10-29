rankhospital <- function(state, outcome, num = "best") {
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
        if (is.na(selOutcome)) stop('Invalid outcomeout. Must be one of "heart attack", "heart failure", "pneumonia"')
        if (!state %in% data$State) stop('Invalid state')
        
        ## Return hospital name in that state with the given rank
        if (selOutcome==1) {
                data <- data[!is.na(data$heartattack) & data$State == state,]
                rnk <- order(data$heartattack,data$Hospital.Name)
        }
        else if (selOutcome==2) {
                data <- data[!is.na(data$heartfailure) & data$State == state,]
                rnk <- order(data$heartfailure,data$Hospital.Name)
        }
        else if (selOutcome==3) {
                data <- data[!is.na(data$pneumonia) & data$State == state,]
                rnk <- order(data$pneumonia,data$Hospital.Name)
        }
        if (num=="best") num <- 1
        else if (num=="worst") num <- nrow(data)
        data <- data[rnk,2]
        data[num]
        
}