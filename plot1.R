plot1<-function(){
    ##plot test
    outcome<-read.csv("hospitalData/outcome-of-care-measures.csv")
    hist(as.numeric(outcome[,11]))
}