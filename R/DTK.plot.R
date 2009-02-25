`DTK.plot` <-
function(x="DTK.test output"){
out=x[[2]]
a=x[[1]]
n=nrow(out)
plot(c(max(out),min(out)),c(1,n),type='n',xlab="Mean Difference",ylab="Mean Comparison",yaxt='n')
axis(2,at=seq(1,n),labels=rownames(out))
title(main=paste(paste(1-a,"%",sep=""),"Confidence Intervals"))
for (i in 1:n){
maxi<-max(out[,3])
mini<-min(out[,2])

lines(out[i,2:3],y=c(i,i))
points(x=out[i,1],y=i)		
	}
}

