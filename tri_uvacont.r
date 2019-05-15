tri_uvacont<-function(vaa,Id,data,dm=NA,dgt=1){
    # vaa : la variable aléatoire
    # Id : l'identifiant patient
    # data la data
    # dm : codage de la données manquante
    va0<-data[[vaa]]
   
    # traitement de la données manquante
    if(is.na(dm)){va<-va0}else{va<-ifelse(va0==dm,NA,va0)}
    
    # taille moin la donnée manquante
    nbr=length(va[!is.na(va)])
    
    # fonction qui calcul le coefficient de variation
    fct_varcoef<-function(vec){
      # vec : vecteur   
      coefv=(sd(vec)/mean(vec))*100
      if(is.nan(coefv)){
        cofv1=0
      }else{
        cofv1=coefv
      }
      return(cofv1)
    }
    # coeff de variable 
    labva=va[!is.na(va)]
    coefvar=round(fct_varcoef(labva),digits=dgt)
    
    # le signe
    vva<-va[!is.na(va)]
    rrp<-sum(ifelse(vva>=0,"+","-")%in%c("+"))/length(vva)
    if(rrp==1){p2="+"}
    if(rrp==0){p2="-"}
    if(rrp<1){p2="+ or -"}
    
    # Calcul de l'interquartile range
    Q1=quantile(va,prob=c(0.25),na.rm=TRUE)
    Q3=quantile(va,prob=c(0.75),na.rm=TRUE)
    IQR=Q3-Q1
    
    # les bornes interieures
    BintInf=Q1-IQR*1.5
    BintSup=Q3+IQR*1.5
    
    # les bornes exterieurs
    BextInf=Q1-IQR*3
    BextSup=Q3+IQR*3
    
    # Calcul des valeurs aberrantes mineurs
    ind1=which(va<BintInf & va>=BextInf)
    ind2=which(va<BextSup & va>=BintSup)
    if(length(ind1)==0){vabinfmin1=NULL}else{
        vabinfmin1<-as.vector(data[ind1,][[Id]])}
    if(length(ind2)==0){vabsupmin1=NULL}else{
        vabsupmin1<-as.vector(data[ind2,][[Id]])}
    if(length(vabinfmin1)==0){vabinfmin="Neant"}else{vabinfmin=vabinfmin1}
    if(length(vabsupmin1)==0){vabsupmin="Neant"}else{vabsupmin=vabsupmin1}
    
    # Calcul le nombre individus à problème
    IndAbmin<-unique(c(vabinfmin,vabsupmin))
    tt=sum(IndAbmin%in%c("Neant"))/length(IndAbmin)
    if(tt==1){
        nbrmin=0
    }else{
    nbrmin<-length(IndAbmin[IndAbmin!="Neant"])
    }
    # Calcul des valeur aberante majeur
    idx1=which(va<BextInf)
    idx2=which(va>BextSup)
    if(length(idx1)==0){vabinfmaj1=NULL}else{
        vabinfmaj1<-as.vector(data[idx1,][[Id]])}
    if(length(idx2)==0){vabsupmaj1=NULL}else{
        vabsupmaj1<-as.vector(data[idx2,][[Id]])}
    if(length(vabinfmaj1)==0){vabinfmaj="Neant"}else{vabinfmaj=vabinfmaj1}
    if(length(vabsupmaj1)==0){vabsupmaj="Neant"}else{vabsupmaj=vabsupmaj1}
    # nombre d'individu aberrrant majeur
    IndAbmaj<-unique(c(vabinfmaj,vabsupmaj))
    tt=sum(IndAbmaj%in%c("Neant"))/length(IndAbmaj)
    if(tt==1){
        nbrmaj=0
    }else{
        nbrmaj<-length(IndAbmaj[IndAbmaj!="Neant"])
    }
    
    # Resume de la variable
    # Calcul des donnée manquante
      vdm=ifelse(is.na(va),1,0)
      indivDm=as.vector(data[which(is.na(va)),][[Id]])
      Ndm=round(100*sum(vdm)/length(va),digits=2)
      
    # test pour voir si toutes les valeurs de la variables sont identiques
    vt<-sample(vva,length(vva),replace=TRUE)
    idv=sum(ifelse(vt==vva,1,0))/length(vva)
    
    
    # test de la normalité de shapiro wilks
    if(idv==1|length(va)<3){
        p="NA";pp=NA}else{
     if(length(va)>5000){
       va1<-va[sample(5000)]
     }else{va1<-va}
    # test de la normalité (shapiro-wilk)
    pvalue=shapiro.test(va1)$p.value
    pp=round(pvalue,digits=3)
    if(pvalue<=1e-3){p="<0.001"}else{p=paste(round(pvalue,digits=3))}
        }
    # construction de la fonction histogramme
    fctgrah<-function(mylwd=3,mycol=rgb(0,0.5,0.5),myborder=grey(0.8),
        mycolhist=rgb(0,1,1,0.1),mycolnorm=rgb(0.7,0.5,0.7),...){
            # de la fonction
              old.par<-par(no.readonly=TRUE)
              on.exit(par(old.par))
              par(mfrow=c(1,3))
              hva<-hist(va,plot=FALSE)
              dva <- density(va,na.rm=TRUE)
              hist(va, freq = FALSE, border = myborder, col = mycolhist, ylim = c(0, max(hva$density, dva$y)),
              cex.main=0.9,main=paste(paste("Histogramme de ",vaa),"\n",paste("Test de Normalité p-value: ",p)),...)
              lines(dva, lwd = mylwd, col = mycol)
              plot(dva$x, log10(dva$y), type = "l", cex.main=0.9,main = "Coordonnées semi-logarithmiques", xlab = vaa,
              ylab = "log10(Density)", las = 1)
              legend("center", legend = round(skewness(va,na.rm=TRUE), 4), cex = 2, text.col = mycol, bty = "n",
        adj = 0.5)
        lines(dva$x, log10(dnorm(dva$x, mean(va,na.rm=TRUE), sd(va,na.rm=TRUE))), col = mycolnorm, lwd = mylwd)
        qqnorm(va, cex.main=0.9,main = "Droite de Henry", xlab = "Quantiles théoriques",
        ylab = "Quantiles observés", las = 1)
        qqline(va)
    }
    
    # la liste des individu aberrants
    # individu commun
    if(length(IndAbmin)==1){IndAbmin=IndAbmin}else{IndAbmin<-IndAbmin[IndAbmin!="Neant"]}
    if(length(IndAbmaj)==1){IndAbmaj=IndAbmaj}else{IndAbmaj<-IndAbmaj[IndAbmaj!="Neant"]}
    if(length(indivDm)==1){indivDm=indivDm}else{indivDm<-indivDm[indivDm!="Neant"]}
    n1=length(IndAbmin);n2<-length(IndAbmaj)
    if(n1>n2){
        n3=n1-n2
        rrp<-c(IndAbmaj,rep("",n3))
        dt<-cbind(IndAbmin,rrp)
    }else{
        n3=n2-n1
        rrp<-c(IndAbmin,rep("",n3))
        dt<-cbind(rrp,IndAbmaj)
    }
    
     n4<-dim(dt)[1]
     n5<-length(indivDm)
     if(n5>n4){
         n6=n5-n4
         dt1<-rbind(dt,matrix("",nrow=n6,ncol=2))
         dtf<-cbind(dt1,indivDm)
     }else{
         n7=n4-n5
         rrp<-c(indivDm,rep("",n7))
         dtf<-cbind(dt,rrp)
     }
    datf<-as.data.frame(dtf)
    names(datf)<-c(paste("IabMi_",vaa,sep=""),paste("IabMa_",vaa,sep=""),paste("IabMd_",vaa,sep=""))
    # formatage des data
    BextInf<-round(BextInf,digits=dgt);BintInf<-round(BintInf,digits=dgt)
    BintSup<-round(BintSup,digits=dgt);BextSup<-round(BextSup,digits=dgt)
    nbrmin<-round(nbrmin,digits=dgt);nbrmaj<-round(nbrmaj,digits=dgt)
    Ndm<-round(Ndm,digits=dgt)
    # Le Resume de la variable
    vcr<-matrix(c(BextInf,BintInf,BintSup,BextSup,nbrmin,nbrmaj,p2,Ndm,p,nbr,coefvar),nrow=1)
    colnames(vcr)<-c("BextIf","BintIf","BintSp","BextSp","N.Abmin","N.Abmaj","Sign","MD(%)","p-v","N","CV")
    rownames(vcr)<-vaa
    
    return(list(matD=vcr,fcthist=fctgrah,lstIdv=datf))
}
