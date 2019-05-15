Rank_test<-function(tim,even,ssta,data1,coln=NULL,titre1=NULL){
           # la fonction qui permet de faire le test-log-rang
           # tim : le temps de survie 
           # even : statut 
           # ssta : la variable de groupe.
           # coln : couleur
           # titre1 : le titre
	       # la survie par strate
	         fl<-formula(paste("Surv(",tim," , ",even,")"," ~ ",ssta))
	         uu<-levels(data1[[ssta]])
	         ff1<-survfit(fl,data=data1)
	         ff2<-survdiff(fl,data=data1)
	         np<-length(uu)
	         pp<-(np-1)
	         p=round(1-pchisq(ff2$chisq,pp),digits=3)
             pp1=(1-pchisq(ff2$chisq,pp))
	         if(p==0){p1=paste(" <0.001")}else{p1=paste("=",p,sep=" ")}
	 # la fonction grap
	   if(is.null(coln)==TRUE){coln1=c(1:np)}else{coln1=coln}
	   if(is.null(titre1)==TRUE){tt=ssta}else{tt=titre1}
	 f1p<-function(x,y,namxylab=NULL){
         if(is.null(namxylab)==TRUE){
		                             plot(ff1,col=coln1,lty=rep(1,np),lwd=rep(2,np),xlab="Time",ylab="Survival")
         }else{plot(ff1,col=coln1,lty=rep(1,np),lwd=rep(2,np),xlab=namxylab[1],ylab=namxylab[2])
         }
		legend(x,y,uu,col=coln1,lty=rep(1,np),lwd=rep(2,np))
		title(main=paste(paste("Log-Rank Test:",tt,sep=" "),"\n",sep=" "),cex.main=1.4)
        #title(main=paste("\n","H0 : No difference between survival curve",sep=" "),cex.main=1)
        title(main=paste("\n","H0: no difference, P-value:",p1,sep=" "),cex.main=1)
        }
    list(fup=f1p,ff1=ff1,rpv=pp1)    
	}
	
