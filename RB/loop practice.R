#Loop practice!

for(this_number in 1:10) {
    cat("Here's number", this_number, "\n")
    
}


for(this_number in 1:10000) {
    if(this_number %% 1000 == 0) {cat("starting number", this_number, "\n" )
    }
    result = this_number * 3
}

#modulo is integer division


mylist = list(0, "cheese", TRUE)

for(this_thing in mylist) {
    cat(this_thing)
}

for(this_thing in mylist) {
    if(is.logical (this_thing)) {
        cat("this is logical\n")
    } else{
        cat("this is not logical \n")
    }
}



for(this_thing in mylist) {
    if(is.logical (this_thing)) {
        cat("this is logical\n")
    } else if (is.numeric(this_thing)) {
        cat("this is not logical, but it's a number \n")
    } else {
        cat("this is not very logical \n")
    }
        
}


#julianne's issue

disfluencies = data.frame (ID = c(1,2), 
                           word = c(5, 3), 
                           um = c(2, 1))



disfluencies$total = apply (disfluencies [, 2:3], 1, sum)

sapply(disfluencies$total, 
       function(x) rep(disfluencies$ID, each = x))

make_rows = function(ID_chunk) {
    output = data.frame(ID = unique(ID_chunk$ID), 
                        response = c(rep(0, ID_chunk$word), 
                                     rep(1,ID_chunk$um)))
    output
}
