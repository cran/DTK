`DTK.test` <-
function(x="data vector",f="factor vector",a="alpha level"){
	#DEFAULT
	if (a=="alpha level"){a=0.05}
	if (class(f)!="factor"){f=factor(f)}
	#Factor Level Names
	Level.Name=levels(f)
	k=length(Level.Name)
#removing NA values
	input=cbind(f[!sapply(is.na(x),all)],x[!sapply(is.na(x),all)])
	input=input[order(input[,1]),]
	f=factor(input[,1])
	x=input[,2]
	
#calculating sample size vector
	n=numeric(length=nlevels(f))
	for (i in 1:length(n)){
	n[i]=length(f[f==i])
	}
	
#degrees of freedom
	for (i in 1:length(n)){
		if (i==1){df=numeric(length=length(n))}
		df[i]=n[i]-1
		}

#sample standard deviation
	for (i in 1:nlevels(f)){
		if (i==1){s=numeric(length=nlevels(f))}
		s[i]=sd(x[f==i])
		}

#studentized scalars (Studentized Range Distribution)
	SR=qtukey(p=a,nmeans=k,df=df,lower.tail=FALSE)
	
#vstar
	vstar=(s^2)/n
	
#SRstar
	SRstar=array(NA,c(nlevels(f),nlevels(f)))
	for (i in 1:nrow(SRstar)){
		for (j in 1:ncol(SRstar)){
			SRstar[i,j]=(SR[i]*vstar[i]+SR[j]*vstar[j])/(vstar[i]+vstar[j])
			}
		}
	
#Pairwise average weighted scalars
	A=SRstar/sqrt(2)
	A=A[lower.tri(A)==TRUE]

#Matrix of CI values
	CI=array(NA,c(nlevels(f),nlevels(f)))
	for (i in 1:nrow(SRstar)){
		for (j in 1:ncol(SRstar)){
			CI[i,j]=A[i]*(sqrt(vstar[i]+vstar[j]))
			}
		}
	CI=CI[lower.tri(CI)==TRUE]
#Mean Vector
	mu=numeric(length=nlevels(f))
	for (i in 1:length(mu)){
		mu[i]=mean(x[f==i])
		}
	for (i in 1:length(mu)){
	if (i==1){mu.dif=array(NA,c(length(mu),length(mu)))}
	for (j in 1:length(mu)){
		mu.dif[i,j]=mu[i]-mu[j]
		mu.dif[j,i]=mu[j]-mu[i]
		}
	}
	#pull out lower matrix
	mu.dif=mu.dif[lower.tri(mu.dif)==TRUE]
	
#Lower and Upper CI bounds		
	u=mu.dif+CI
	l=mu.dif-CI

#vector of comparison names

	for (i in 1:nrow(SRstar)){if (i==1){c.n=SRstar*0} 
		for (j in 1:ncol(SRstar)){c.n[i,j]=j}}
	for (i in 1:nrow(SRstar)){if (i==1){r.n=SRstar*0} 
		for (j in 1:ncol(SRstar)){r.n[i,j]=i}}
	r.n=r.n[lower.tri(r.n)==TRUE]
	c.n=c.n[lower.tri(c.n)==TRUE]
	combo=paste(r.n,c.n,sep="-")

out=cbind(mu.dif,l,u)
colnames(out)=c("Diff","Lower CI","Upper CI")
rownames(out)=combo
out=list(a,out)
return(out)
	
	}

