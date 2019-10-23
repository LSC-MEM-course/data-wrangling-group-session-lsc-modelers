for(this_number in 1:10000) {
    if(this_number %% 1000 == 0) {
        cat("starting number", this_number, "\n")
    }
    result <- this_number * 3
}

mylist <- list(0, "cheese", TRUE)
for(this_thing in mylist){
    if(is.logical(this_thing)) {
        cat("this is very logical\n")
    } else {
        cat("this is not very logical\n")
    }
}
