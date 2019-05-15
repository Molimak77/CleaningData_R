Desc_fctUn<-function(vari,data,outcom=NULL,namv=NULL,Md=NA,prci=1){
         # Fonction qui permet de descrire une variable qualitative  
         # Auteur : Molière Nguile-Maka
         # Version : v.02 du 2017/10/02
         # vari : le nom de la variable.
         # data : la base de données 
         # outcom : la variable cible data les donnés (la outcomme est binaire)
         # namv: les nouvelles variable
         # commentaire : donner à l'utilisateur de choisir son test
         
         if(is.factor(data[[vari]])==TRUE){
             nvar<-data[[vari]]
             levels(nvar)[levels(nvar)==Md]<-NA
             data[[vari]]<-nvar
             # resumé brute
             ssl<-summary(data[[vari]])
             rsv<-t(as.matrix(ssl))
             rang=which(labels(ssl)=="NA's")
             if(length(rang)!=0){
                 nam<-c(colnames(rsv)[-rang],"NA(%)")
                 nas<-rsv[,rang];rsv1<-rsv[,-rang]
                 tabg<-round(cbind(t(rsv1),t(100*(nas/length(data[[vari]])))),digits=prci)
             }else{
                 nam<-c(colnames(rsv),"NA(%)")
                 tabg<-round(cbind(t(t(rsv)),t(0)),digits=prci)
             }
          
             colnames(tabg)<-nam
             rownames(tabg)<-vari
             if(is.null(outcom)==FALSE){
                 if(is.numeric(data[[outcom]])|is.integer(data[[outcom]])){
                     vv<-data[[outcom]]
                     vv[vv==Md]<-NA
                     data[[outcom]]<-as.factor(vv)
                 }else{
                     if(is.character(data[[outcom]])){
                         vv2<-data[[outcom]]
                         vv2[vv2==Md]<-NA
                          data[[outcom]]<-as.factor(vv2)
                     }else{
                         if(is.factor(data[[outcom]])){
                             vv2<-data[[outcom]]
                             levels(vv2)[levels(vv2)==Md]<-NA
                             data[[outcom]]<-vv2
                         }else{
                             stop(paste("The outcom is of the class ",class(data[[outcom]])," change to a factor"))
                         }
                     }
                 }
                        data1=data[!is.na(data[[outcom]]),]
                        # Connaître le nombre de niveau
                        tt<-table(data1[[vari]],data1[[outcom]])
                          ttp<-round(prop.table(tt, 2),digits=prci)
                          sumM<-margin.table(tt, 1)
                 
                        # missing data
                          tt1<-table(data[[vari]],data[[outcom]],exclude=NULL)
                          ttp1<-round(prop.table(tt1, 2),digits=prci)
                          sumM1<-margin.table(tt1, 2)
                 
                        # Description des tables
                        #vec1<-unlist(labels(tt)[[2]])
                          vec1<-levels(data[[outcom]])
                          TabR<-NULL;i=1 ;tabE<-NULL;nam<-NULL
                          
                          for(u in vec1){
                                        nam<-c(nam,paste(u,c("(n)","(%)"),sep=" "))
                                        TabR<-cbind(TabR,as.vector(tt[,i]))
                                        TabR<-cbind(TabR,as.vector(ttp[,i])*100)
                                        tabE<-cbind(tabE,as.vector(tt[,i]))
                                        i=i+1
                                        }
                          rro<-try(fisher.test(tabE),TRUE)
                          if(inherits(rro,"try-error")==TRUE){
                              Xsq <- chisq.test(tabE)
                          }else{Xsq <- fisher.test(tabE)}
                          
                          uu<-rep("",dim(tabE)[1])
                          uu[1]<-round(Xsq$p.value,digits=3)
                          
                           nam1<-c("Varibles","Class",nam,"P_value","Nbr")
                          if(is.null(namv)==TRUE){
                          vari1=vari
                          }else{vari1=namv}#naml<-namv}
                          
                          naml<-unlist(labels(table(data[[vari]])))
                          nvm<-c(vari1,rep("",length(naml)-1))
                          rownames(TabR)<-NULL
                          TabR<-round(TabR,digits=prci)
                          naml1<-as.vector(naml)
                          matR<-data.frame(nvm,naml1,TabR,uu,as.vector(sumM))
                          names(matR)<-NULL
                          names(matR)<-nam1
                          
                 }else{
                       # Connaître le nombre de niveau
                          tt<-table(data[[vari]])
                          ttp<-prop.table(tt)*100
                          sumM<-margin.table(tt)
                          nbr<-as.vector(tt)
                          pct<-round(as.vector(ttp),digits=prci)
                          nbrp<-rep(as.numeric(sumM),length(nbr))
                          if(is.null(namv)==TRUE){
                          	vari1=vari
                          }else{vari1<-namv}
                          naml1<-unlist(labels(table(data[[vari]])))
                          nvm<-c(vari1,rep("",length(naml1)-1))
                          naml2<-as.vector(naml1)
                          matR<-data.frame(nvm,naml2,nbr,pct,nbrp)
                          names(matR)<-NULL
                          names(matR)<-c("Varibles","Class","n","(%)","Nbr")
                          
                          tt1<-table(data[[vari]],exclude=NULL)
                          ttp1<-prop.table(tt1)
                          sumM1<-margin.table(tt1)

                         } 
            return(list(matR=matR,matNA=tt1,ech=sumM1,TRb=tabg))
               }else{print(vari)
            stop(paste("The",vari,"is not factor",sep=" "))
         }
         }
         
