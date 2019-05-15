 Desc_fctMu<-function(vardsc,data,outcom1=NULL,namv1=NULL,VMd=NA,prc=1){
             # Fonction qui fait la description des facteurs 
             # version v.02 début : 2014/10/03
             # version v.03 début : 2018/05/29
             # auteur : Molière Nguile-Makao
             # vardsc: vecteur des variables 
             # data : la fonction qui fait la description des facteur
             # option : outcom1 : la variable cible.
             # option : namv1 : les noms des variables
             # option : prc : le precision de resultats
             # Test utilisé est de comparaison de proportion du khi-2.
             # etape 1
             pp<-sum(vardsc%in%names(data))-length(vardsc)
             pp1<-abs(pp)
             if(pp<0){print(vardsc)
                 stop(paste("Il y a ",pp1," variable(S) qui n'appartient pas aux données",sep=" "))
             }else{
                 tabR<-NULL;lst_Mis<-list();ech<-list();TRb<-list();i=1
                 for(u in vardsc){
                     #print(u)
                          ll<-Desc_fctUn(u,data,outcom=outcom1,namv=namv1[i],Md=VMd[i],prci=prc)
                          tabR<-rbind(tabR,ll$matR)
                          lst_Mis[[i]]<-ll$matNA
                          ech[[i]]<-ll$ech
                          TRb[[i]]<-ll$TRb
                          i=i+1
                        }
                 names(lst_Mis)<-vardsc
                 names(ech)<-vardsc
                 names(TRb)<-vardsc
                 return(list(tabR=tabR,lst_Mis=lst_Mis,ech=ech,outc=outcom1,TRb=TRb))
                 }
                  }
             
