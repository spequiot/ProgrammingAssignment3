## Returns the hospital having the lowest 30-mortality rate for a given State and outcome
## Outcome can be one of "heart attack", "heart failure", "pneumonia"
## Data read from  outcome-of-care-measures.csv
## Column to read : 
##    Hospital.Name
##    State
##    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, col 11
##    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, col 17
##    Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, col 23

best <- function(state,outcome){
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
        
        ##Return hospital name in that state with lowest 30-days death
        if (selOutcome==1) {
                data <- data[!is.na(data$heartattack) & data$State == state,]
                deathmin <- min(data$heartattack)
                bests <- data[data$heartattack==deathmin,] 
        }
        else if (selOutcome==2) {
                data <- data[!is.na(data$heartfailure) & data$State == state,]
                deathmin <- min(data$heartfailure)
                bests <- data[data$heartfailure==deathmin,]
        }
        else if (selOutcome==3) {
                data <- data[!is.na(data$pneumonia) & data$State == state,]
                deathmin <- min(data$pneumonia)
                bests <- data[data$pneumonia==deathmin,]
        }
        best <- min(bests$Hospital.Name)
        
        best
}