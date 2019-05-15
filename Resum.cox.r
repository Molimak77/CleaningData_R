# la fonction qui resume le modèle de Cox 
# auteur M. Nguile-Makao
# v_02 du 02/07/18
       
       
Resum.cox<-function(object){ 
             # object : object résultant du modèle de Cox 
             ll<-summary(object)
             tir<-rownames(ll$conf.int)
             tir1=unlist(strsplit(tir,"[:]"))
             if(length(tir)==length(tir1)){
          # recherche de la ref
             ss<-terms(object)
             varI<-attr(ss,"term.labels")[1]
             if(as.vector(attr(ss,"dataClasses")[varI])=="factor"){
                 rr<-object[["xlevels"]][[varI]]
                 vaf<-c(varI,rep("",length(rr)-1));fact=1
             
             }else{
                 fact=NULL
                 rr="1";
                 vaf<-varI
             }
             
             mat<-ll$conf.int[,-2]
             p_value<-ll[[7]][,5]
             if(is.null(dim(mat))==TRUE){
             mat1<-t(mat)
             }else{mat1<-mat}
             
             # prepartion de la table
             Rst<-cbind(mat1,p_value)
             if(is.null(fact)==TRUE){
                 Rst1<-Rst
             }else{Rst1<-rbind(c(1,NA,NA,NA),Rst)}
             # extration de la pv-globle
             if(dim(Rst1)[1]==1){
                 if(Rst1[4]<1e-4){pvs<-"<0.001"}else{pvs<-as.character(round(Rst1[4],3))}
                 if(ll$wal[3]<1e-4){pvg<-"<0.001"}else{pvg<-as.character(round(ll$wal[3],3))}
             }else{
                 pvs<-ifelse(Rst1[,4]<1e-4,"<0.001",as.character(round(Rst1[,4],3)))
                 if(ll$wal[3]<1e-4){pvg<-"<0.001"}else{pvg<-as.character(round(ll$wal[3],3))}
             }
             # resultat
             Rst2<-round(Rst1[,-dim(Rst1)[2]],digits=1)
             if(is.null(fact)){
                 Rst2<-cbind(t(as.matrix(Rst2)),pvs)
             }else{
                 Rst2<-cbind(t(t(as.matrix(Rst2))),pvs)
             }
             if(dim(Rst2)[1]==1){
                 Rst2=Rst2
             }else{
                 Rst2[1,2]<-"--"
                 Rst2[1,3]<-"--"
                 Rst2[1,4]<-"--"
             }
             colnames(Rst2)<-NULL
             rownames(Rst2)<-NULL
             Rst3<-data.frame(vaf,rr,Rst2)
             colnames(Rst3)<-c("Cov","levels","HR","IC(2.5%)","IC(97.5%)","p_value")
             }else{
                 ss<-terms(object)
                 varI<-attr(ss,"term.labels")[2]
                 varI1<-attr(ss,"term.labels")[1]
                 mat<-ll$conf.int[,-2][-1,]
                 col1<-c(varI,rep("",dim(mat)[1]-1))
                 col2<-as.vector(sapply(rownames(mat),function(x)substr(x,nchar(varI1)+1,nchar(x))))
                 p_value<-round(as.vector(ll[[7]][,5][-1]),digits=3)
                 mat1=mat;
                 mat2<-round(mat,digits=1)
                 mat3<-cbind(mat2,p_value)
                 colnames(mat3)<-NULL;rownames(mat3)<-NULL
                 Rst3<-data.frame(col1,col2,mat2,p_value)
                 colnames(Rst3)<-c("Cov","levels","HR","IC(2.5%)","IC(97.5%)","p_value")
                 pvg<-p_value
             }
             
            return(list(Rst3=Rst3,pvg=pvg))
             }
