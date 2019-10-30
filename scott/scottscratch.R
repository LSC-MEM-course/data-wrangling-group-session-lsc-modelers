# scratch work during group session

# some loop practice

for(this_number in 1:10000) {
    if(this_number %% 1000 == 0) { 
        cat("starting number", this_number, "\n") 
    }
    result <- this_number * 3
}

mylist <- list(0, "cheese", TRUE)
for(this_thing in mylist) {
    if(is.logical(this_thing)) {
        cat("this is very logical\n")
    } else if(is.numeric(this_thing)) {
        cat("this is not very logical, but at least it's a number\n")
    } else {
        cat("this is not very logical\n")
    }
}

# Julianne's data issue
library(tidyverse)
disfluencies <- data.frame(ID = c(1, 2),
                           word = c(5, 3),
                           um = c(2, 1))

ID_chunk <- disfluencies %>% filter(ID == 2)
make_rows <- function(ID_chunk) {
    output <- data.frame(ID = unique(ID_chunk$ID),
                         response = c(rep(0, ID_chunk$word),
                                      rep(1, ID_chunk$um)))
    output
}

dislong <- disfluencies %>% group_by(ID) %>%
    do(make_rows(.))
dislong
