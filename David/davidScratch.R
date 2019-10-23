install.packages("tidyverse")

library(dplyr)

for(i in 1:10000) {
    if(i %% 1000 == 0) {
        cat("here's a number", i, "\n")
    }
    result <- i*3
}
