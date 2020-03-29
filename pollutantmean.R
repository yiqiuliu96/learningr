pollutantmean <- function(directory, pollutent, id = 1:332) {
        #set the working directory to the folder in which data files are stored
        wd <- paste("D:/File/R Projects/learningr", directory, sep = "/")
        setwd(wd) 
        #the initial value of the key objects in the function
        n <- 0
        s <- 0 
        #iterate to get the sum of all non-NA number and the number of non-NA observations
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
                vec <- data[pollutent]
                realvec <- vec[!is.na(vec)]
                n <- n + length(realvec) #count the number of observations
                s <- s + sum(realvec, na.rm=TRUE) #count the sum of observations
        }
        s / n
}

