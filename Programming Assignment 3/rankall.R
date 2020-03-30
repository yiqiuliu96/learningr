rankall <- function(outcome, num = "best") {
        source('D:/File/R Projects/learningr/Programming Assignment 3/rankhospital.R')
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        hospital <- c(rep(NA, 54))
        st <- c(rep(NA, 54))
        
        s <- unique(data[ , 7])
        n <- 1
        for (i in s) {
                r <- rankhospital(i, outcome, num)
                if (!is.na(r)) {
                        hospital[n] <- r
                        st[n] <- i
                        n <- n+1
                }
                result <- data.frame("hospital" = hospital, "state" = st)
        }
        ## Clean the data
        #data[ , 11] <- as.numeric(data[ , 11])
        #data[ , 17] <- as.numeric(data[ , 17])
        #data[ , 23] <- as.numeric(data[ , 23])
        ## Extract data
        #basic <- data[ , c(2, 7)]
        #ha <- data[ , 11]
        #hf <- data[ , 17]
        #pn <- data[ , 23]
        ## Construct clean data for heart attack
        #hab <- data.frame(basic, "mortality" = ha)
        #hab <- hab[!is.na(hab$mortality), ]
        #or.hab <- order(hab$mortality, hab$Hospital.Name, decreasing = FALSE)
        #ohab <- hab[or.hab, ]
        ## Construct clean data for heart failure
        #hfb <- data.frame(basic, "mortality" = hf)
        #hfb <- hfb[!is.na(hfb$mortality), ]
        #or.hfb <- order(hfb$mortality, hfb$Hospital.Name, decreasing = FALSE)
        #ohfb <- hfb[or.hfb, ]
        ## Construct clean data for pneumonia
        #pnb <- data.frame(basic, "mortality" = pn)
        #pnb <- pnb[!is.na(pnb$mortality), ]
        #or.pnb <- order(pnb$mortality, pnb$Hospital.Name, decreasing = FALSE)
        #opnb <- pnb[or.pnb, ]
        
        
}