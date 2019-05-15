# fonction qui résume le modèle multi-varié de cox 
# auteur M. Nguile-Makao
# version v.01 
      Resum.mcox<-function(object,varI=NULL){
      	              # le type permet de dire la cathégorie de la variable
      	              # object : object resultant du modèle de cox 
      	              # varI : variable d'interêt.
      	              # data1 : la base de donnée.
                      ll<-summary(object)
                      nzz<-rownames(coef(ll))
                      matd<-ll$conf.int
                      if(is.null(varI)){
                          # the all variable
                          all_var<-attr(terms(object),"term.labels")
                          rr<-object[["xlevels"]]
                          if(sum(grep("[:]",nzz))!=0){
                             vait=nzz[grep("[:]",nzz)]
                             all_var1=all_var[-grep("[:]",all_var)]
                             all_var2=all_var[grep("[:]",all_var)]
                             vv1=unlist(strsplit(all_var2,"[:]"))[1]
                             col2<-as.vector(sapply(vait,function(x)substr(x,nchar(vv1)+1,nchar(x))))
                             llconf1=matd[-grep("[:]",nzz),]
                             llconf2=round(matd[grep("[:]",nzz),][,-2],digits=1)
                             pv=as.vector(round(ll$coef[,5][grep("[:]",nzz)],digits=3))
                             llconf2=cbind(llconf2,pv)
                             rownames(llconf2)<-NULL
                             col1=c(all_var2,rep("",dim(llconf2)[1]-1))
                             llconf2<-data.frame(col1,col2,llconf2)
                             macof=ll$coef[-grep("[:]",nzz),]
                          }else{
                              vait=NULL
                              all_var1=all_var
                              llconf1=matd
                              llconf2=NULL
                              macof=ll$coef
                          }
                         matR<-NULL
                         for(ux in all_var1){
                             if(ux%in%names(rr)){
                                 vv<-rr[[ux]]
                                 varmod<-vv
                                 nvarmod<-c(ux,rep("",length(vv)-1))
                                 if(length(vv)==2){
                                     pv0<-macof[grep(ux,rownames(macof)),][5]
                                     if(pv0<1e-3){pv=c(NA,"<0.001")}else{pv<-c(NA,as.character(round(pv0,digits=3)))}
                                     Hr<-round(rbind(c(1,NA,NA),t(llconf1[grep(ux,rownames(llconf1)),][-2])),digits=1)
                                     }else{
                                          pv0<-macof[grep(ux,rownames(macof)),][,5]
                                          pv<-c(NA,as.vector(ifelse(pv0<1e-3,"<0.001",as.character(round(pv0,digits=3)))))
                                          Hr<-round(rbind(c(1,NA,NA),llconf1[grep(ux,rownames(llconf1)),][,-2]),digits=1)
                                          }
                             }else{
                                 varmod<-ux
                                 nvarmod<-ux
                                 pv0<-macof[grep(ux,rownames(macof)),][5]
                                 if(pv0<1e-3){pv="<0.001"}else{pv<-as.character(round(pv0,digits=3))}
                                 Hr<-t(round(llconf1[grep(ux,rownames(llconf1)),][-2],digits=1))
                                 }
                            matR<-rbind(matR,data.frame(nvarmod,varmod,Hr,pv))
                         }
                         if(sum(grep("[:]",nzz))!=0){
                         colnames(matR)<-NULL
                         colnames(llconf2)<-NULL
                         ccol1<-as.vector(unlist(c(matR[1],llconf2[1])))
                         ccol2<-as.vector(unlist(c(matR[2],llconf2[2])))
                         ccol3<-as.vector(unlist(c(matR[3],llconf2[3])))
                         ccol4<-as.vector(unlist(c(matR[4],llconf2[4])))
                         ccol5<-as.vector(unlist(c(matR[5],llconf2[5])))
                         ccol6<-c(t(t(matR[6])),t(t(llconf2[6])))
                       matR=data.frame(ccol1,ccol2,ccol3,ccol4,ccol5,ccol6)
                         }else{
                          matR=matR
                         }
                      }else{
      	             
                      # recherche de la ref
                      ss<-terms(object)
                      # construction des label
                      if(as.vector(attr(ss,"dataClasses")[varI])=="factor"){
                          rr<-object[["xlevels"]][[varI]]
                          if(length(rr)>2){
                          vaf<-c(varI,rep("",length(rr)-1))
                          rs<-rbind(c(1,NA,NA),round(ll$conf.int[grep(varI,nzz),][,-2],digits=1))
                          p_value<-c(NA,as.character(ifelse(ll[[7]][,5][grep(varI,nzz)]<1e-3,"<0.001",round(ll[[7]][,5][grep(varI,nzz)],digits=3))))
                          }else{
                              vaf<-c(varI,"")
                              rs<-rbind(c(1,NA,NA),round(ll$conf.int[grep(varI,nzz),][-2],digits=1))
                              p_value<-c(NA,as.character(ifelse(ll[[7]][,5][grep(varI,nzz)]<1e-3,"<0.001",round(ll[[7]][,5][grep(varI,nzz)],digits=3))))
                          }
                          matR<-data.frame(vaf,rr,rs,p_value)
                        }else{
                            vaf<-t(varI)
                            rr<-t("1")
                            # construction des resultats
                            rs<-t(round(ll$conf.int[grep(varI,nzz),][-2],digits=1))
                            p_value<-t(as.character(ifelse(ll[[7]][,5][grep(varI,nzz)]<1e-3,"<0.001",round(ll[[7]][,5][grep(varI,nzz)],digits=3))))
                            matR<-data.frame(vaf,rr,rs,p_value)
                         }
                      }
                      if(dim(matR)[1]==1){
                         matR=matR
                      }else{
                         matR[1,4]<-"--"
                         matR[1,5]<-"--"
                         #levels(vstgc)[levels(vstgc)%in%c("cT2","cT3")]<-"cT2&cT3"
                         levels(matR[1,6])[levels(matR[1,6])]<-"--"
                      }
                      rownames(matR)<-NULL
                      colnames(matR)<-NULL
                      names(matR)<-c("Covariates","Levels","HR","IC(2.5%)","IC(97.5%)","p_value")
                      return(matR)
                      }
