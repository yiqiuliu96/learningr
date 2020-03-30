best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[ , 11] <- as.numeric(data[ , 11])
        data[ , 17] <- as.numeric(data[ , 17])
        data[ , 23] <- as.numeric(data[ , 23])
        ## Check that state and outcome are valid
        if (state %in% data$State) {
                if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                        if (outcome == "heart attack") {
                                data2 <- data[, c(2, 7, 11)] ## Extract columns of focus
                                correct <- state == data2$State ## Generate a logic vector indicating the correct state
                                data3 <- data.frame(data2, correct) ## Combine the data with the above logic indicator
                                data4 <- data3[data3$correct, ] ## Obtain the data set for the required state
                                or <- order(data4$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data4$Hospital.Name, decreasing = FALSE)
                                data5 <- data4[or, ] ## Order the data
                                ## Return hospital name in that state with lowest 30-day death
                                print(data5[1, 1])
                        }
                        else {
                                if (outcome == "heart failure") {
                                        data2 <- data[, c(2, 7, 17)] ## Extract columns of focus
                                        correct <- state == data2$State ## Generate a logic vector indicating the correct state
                                        data3 <- data.frame(data2, correct) ## Combine the data with the above logic indicator
                                        data4 <- data3[data3$correct, ] ## Obtain the data set for the required state
                                        or <- order(data4$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data4$Hospital.Name, decreasing = FALSE)
                                        data5 <- data4[or, ] ## Order the data
                                        ## Return hospital name in that state with lowest 30-day death
                                        print(data5[1, 1])
                                }
                                else {
                                        data2 <- data[, c(2, 7, 23)] ## Extract columns of focus
                                        correct <- state == data2$State ## Generate a logic vector indicating the correct state
                                        data3 <- data.frame(data2, correct) ## Combine the data with the above logic indicator
                                        data4 <- data3[data3$correct, ] ## Obtain the data set for the required state
                                        or <- order(data4$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data4$Hospital.Name, decreasing = FALSE)
                                        data5 <- data4[or, ] ## Order the data
                                        ## Return hospital name in that state with lowest 30-day death
                                        print(data5[1, 1])
                                }
                                }
                }
                else {
                        mes <- paste("Error in best(", state, ",", outcome, ") : invalid outcome", sep = "'")
                        message(mes)
                }
        }
        else {
                mes <- paste("Error in best(", state, ",", outcome, ") : invalid state", sep = "'")
                message(mes)
        }
}