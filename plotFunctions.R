insideLabel.barplot<-function(X,Main="",Ylab="", Col=rainbow(n=length(X), alpha=0.5)){
   if(length(X)==0){
      empty.plot(Main,OutputFile)
      return()
   }
   op <- par(mar = c(2, 4, 4, 2) + 0.1)
   x.pos<-barplot(X,main=Main, ylab=Ylab, names.arg="", col=Col)
   par(srt=90)
   text(x=x.pos, y=0.1, gsub("[.]"," ", names(X)), adj=c(-0.05,0.5))# puts labels inside bars
   par(srt=0)
   par(op)
}

#tidies up the colnames by removing strip+"." from the front and substituting "." for " "
#returns the tidied colnames
tidy.colnames<-function(df, strip){
   gsub("[.]"," ",sub(paste(strip,".",sep=""),"",colnames(df)))
}