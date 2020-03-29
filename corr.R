corr <- function(directory, threshold = 0) {
        #set the working directory to the folder in which data files are stored
        wd <- paste("D:/File/R Projects/learningr", directory, sep = "/")
        setwd(wd)
        #calculate the number of "good" monitors
        nogood <- 0
        for (j in 1:332) {
                ncom <- complete(directory, j)
                if (ncom[1,2] > threshold) {nogood <- nogood + 1}
        }
        vec.correlation <- rep(NA, times = nogood) #create the prototype of the final vector
        iteration <- 0
        #iterate
        for (i in 1:332) {
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
                if (n > threshold) {
                        iteration <- iteration + 1
                        good.nitrate <- nitrate[good]
                        good.sulfate <- sulfate[good]
                        correlation <- cor(good.sulfate, good.nitrate) #calculate the correlation
                        vec.correlation[iteration] <- correlation #fill the number in the final vector
                }
        }
        vec.correlation
}