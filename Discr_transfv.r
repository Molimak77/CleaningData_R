Discr_transfv<-function(va.qua,lst,md=NULL,lab1=NULL,ref1=NULL){
    # la fonction qui regroupe la variable les cathégorie d'une variable qualitative
    # va.qua: la variable qualitative
    # lst : la liste de qui contient les regroupement des classes
    # lab1 : le vecteur des des noms des nouvelles catégorie
    # ref1 : la nouvelle référence.
    
    # Etape 1 test du facteur ====================================
    if(!(is.factor(va.qua)==TRUE || is.character(va.qua)==TRUE)){
        # premier controle si la variable est qualitative
        stop("This variable isn't a factor or character")
    }else{
        # Etape 2 uniformisation de la variable
        if(is.null(md)){
            vaa<-va.qua
        }else{
            if(length(md)==1){
            va.qua[va.qua==md]<-NA
            }else{
             va.qua[va.qua%in%md]<-NA
            }
            vv<-as.vector(unlist(labels(table(va.qua))))
            vv1<-vv[!vv%in%md]
            vaa<-factor(va.qua,labels=vv1)
            }
       # Test de la liste de regroupement
        if(sum(unlist(lst)%in%va.qua)!=length(unlist(lst))){ # petit problème à regler
            zx<-unlist(lst)[which(unlist(lst)%in%va.qua==FALSE)]
            stop(paste("the following values: ",paste(zx,collapse=", ")," don't belong to the variable ",paste(substitute(va.qua)), " Please verfy your grouping"))
        }else{
            # test de pour toutes les valeur de la variable appartiennent à un groupe
            vaaq<-vaa[!is.na(vaa)]
            if(sum(vaaq%in%unlist(lst))!=length(vaaq)){
                mxx<-unique(as.vector(vaaq[which(vaaq%in%unlist(lst)==FALSE)]))
                stop(paste("the following value(s): ",paste(mxx,collapse=", ")," don't have the grouping",sep=""))
            }else{
                # traitement de regroupement
                # petite fonction qui fait tous
                trs1<-function(vaa,lst){
                    nwv1<-NULL;i=1
                    for(ux in 1:length(lst)){
                        nwv1<-cbind(nwv1,ifelse(va.qua%in%lst[[ux]]==TRUE,i,0))
                        i=i+1
                    }
                    nwv1<-cbind(nwv1,ifelse(is.na(vaa),NA,0))
                   return(nwv1)
                }
                # traitement de la variable
                datva<-trs1(vaa,lst)
                vaii<-apply(datva,1,sum)
                
                # traitement des labels
                if(is.null(lab1)){
                 lab2=paste(rep("Group",length(lst)),c(1:length(lst)),sep="")
                }else{
                    if(length(lab1)!=length(lst)){
                        stop("length of the label is different of the length lst")
                    }else{
                    lab2=lab1
                }
                }
                vaii1<-factor(vaii,labels=lab2)
                # traitement de la reference
                if(is.null(ref1)){
                    vaii2<-relevel(vaii1,ref=labels(table(vaii1)[table(vaii1)==min(table(vaii1))]))
                }else{
                    vaii2<-relevel(vaii1,ref=ref1)
                }
                return(vaii2)
            }
        }
    }
}
   
