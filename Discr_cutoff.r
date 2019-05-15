Discr_cutoff<-function(va.co,vec.cutof,label1=NULL,ref1=NULL,md=NULL){
    # la fonction qui discretise une variable continue
    # va.co : la variable qui doit être continue
    # vec.cutof : vecteur des cut-off
    # ref1: est la référence de la variable de la nouvelle variable (NB: c'est un chiffre indiquant la catégorie
    # quand les labels ne sont pas précisé) et le nom d'une cathégorie si les label sont précisé
    # label1 : les noms des cathégorie créer
    # md : missing data
    # decoupage par rappor au cutoff c1 et c2, x : la variable se fait de la façon suivante
    # si x<=c1 1 sinon 0
    # si x>c1 et x<=c2 2 sinon 0
    # si x>c2 3 sinon 0
    # par la suite on fait une sommation en tenant compte des NA 
    # etape test numeric de la variable
    if(is.numeric(va.co)==FALSE){
        stop("This variable is not a continous variable")
    }else{
        # test du cut-off numeric
        if(is.numeric(vec.cutof)==FALSE | length(vec.cutof)>length(va.co)){
            stop(paste("The cut-off vector is not numeric vector or the length of the cut-off vector > length of ",paste(substitute(va.co))))
        }else{
            # uniformisation de la variable
            if(is.null(md)){
                vaco<-va.co
            }else{
                if(length(md)==1){
                    vaco<-ifelse(va.co==md,NA,va.co)
                }else{
                    vaco<-ifelse(va.co%in%md,NA,va.co)
                }
            }
            # la decoupe des variables
            vec.cutof1=sort(vec.cutof)
            veco1<-ifelse(is.na(vaco)==TRUE,NA,0);
            ll<-cbind(c(-Inf,vec.cutof1),c(vec.cutof1,Inf))
            vec11<-ifelse(vaco<=ll[1,][2],1,0);
            vec12<-ifelse(vaco>ll[dim(ll)[1],][1],dim(ll)[1],0)
            veclab1<-c(paste(paste(substitute(va.co))," in ]",round(min(vaco,na.rm=TRUE))-1," , ",ll[1,][2],"]",sep=""))
            veclab2<-paste(paste(substitute(va.co))," in ]",ll[dim(ll)[1],][1]," , ",round(max(vaco,na.rm=TRUE))+1,"[",sep="")
             pp<-(dim(ll)[1]-1)
             if(pp<2){
                 ma.va<-cbind(vec11,vec12)
                 vc_lab=c(veclab1,veclab2)
             }else{
                 veclab<-NULL
                 vec1<-NULL
                 i=2
                 for(uu in 2:pp){
                     vec1<-cbind(vec1,ifelse(ll[uu,][1]<vaco & vaco<=ll[uu,][2],i,0))
                     veclab<-c(veclab,paste(paste(substitute(va.co)),"in ]",ll[uu,][1]," , ",ll[uu,][2],"]",sep=""))
                     i=i+1
                 }
                 ma.va<-cbind(vec11,vec1,vec12)
                 vc_lab=c(veclab1,veclab,veclab2)
             }
             # variable final
             va_bin<-apply(ma.va,1,sum)
             # test sur labels
             if(is.null(label1)){
                 vaii<-factor(va_bin,labels=vc_lab)
             }else{
                 # verification du bon label
                 if(length(label1)!=length(unlist(labels(table(va_bin))))){
                     stop("the length of the label vector is not equal to the length of level variable ")
                 }else{
                     if(!(is.factor(label1)|is.character(label1))){
                         stop("the vector of the labels is probably the numeric vector ")
                     }else{
                         vaii<-factor(va_bin,labels=label1)
                     }
                 }
              }
             # test de la reférenec
             if(is.null(ref1)){
                 vaii1<-relevel(vaii,ref=labels(table(va_bin)[table(va_bin)==min(table(va_bin))]))
             }else{
                 vaii1<-relevel(vaii,ref=ref1)
             }
         return(vaii1)
    }
  }
}
 
  
 
          
       
        
