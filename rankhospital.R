rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data.raw <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", colClasses = "character")
    
    ## Check that state and outcome are valid
    data.raw$State <-as.factor(data.raw$State)
    
    ## Is the state included in the state list of the data?
    test=data.raw$State[grep(state, data.raw$State, ignore.case=F)]
    if (length(test) == 0) {
        stop("invalid state")
    }
    
    ## Is the outcome one of the three expected types?
    if (outcome == "heart attack") {
        outCol <- 11
    } else if (outcome ==  "pneumonia") {
        outCol <-23
    } else if (outcome == "heart failure") {
        outCol <- 17
    } else {
        stop("invalid outcome")
    }
    
    # Extract the appropriate columns and clean the data
    names <-c("State","Hospital.Name",names(data.raw[outCol]))
    data <- subset(data.raw, data.raw$State==state, select=c(names[1],names[2],names[3]))
    data[3] <- as.numeric(data[,3])
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    sort.data <- data[order(data[3],data[2],na.last=NA),]
}