evaluteSourceExpressions <- function(file){
    coms <- parse(file)
    for (i in seq_along(coms)){
        print(coms[[i]])
        eval(coms[[i]],envir=.GlobalEnv)
        mess <- paste("Expression",i,"of",length(coms),"parsed. Press <return> to continue.")
        cat(mess)
        readLines(n=1)
    }
}