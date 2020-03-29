complete <- function(directory, id = 1:332) {
        #set the working directory to the folder in which data files are stored
        wd <- paste("D:/File/R Projects/learningr", directory, sep = "/")
        setwd(wd)
        result <- data.frame("id" = id, "nobs" = NA) #create the final data frame
        iteration <- 0
        #iterate
        for (i in id[seq_along(id)]) {
                #construct the file name
                if (i < 10) {
                        name <- paste("00", i, ".csv", sep = "")}
                else {
                        if (i < 100) {
                                name <- paste("0", i, ".csv", sep = "")
                        }
                        else {
                                name <- paste(i, ".csv", sep = "")
                        }
                } 
                data <- read.csv(name)
                nitrate <- data["nitrate"]
                sulfate <- data["sulfate"] #extract two vector of PMs
                good <- (is.na(nitrate) == is.na(sulfate)) & is.na(nitrate) == FALSE #indicate if each obervation is complete
                n <- sum(good)
                iteration <- iteration + 1
                result[iteration, 2] <- n
        }
        result
}