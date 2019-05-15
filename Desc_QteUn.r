Desc_QteUn<-function(vardsc,data=NULL,outcom=NULL,pre=2,test=1){
    # test : indique le sens de l'équation soit y en catégo et x en continue y~x (test=2) et x~y (test=1)
    # pour que kruskal w soit egale wilcoxon il faut écrire x~y au cas ou y a plus de 2 catégorie
       # Fonction qui permet de faire la description des variables continues 
       # vardsc : vecteur des noms des variables 
       # data : les data
       # outcome : la outcome 
       # namv: nom des variables
       # auteur Molière Nguile-Makao 
       # version V.01 06/03/15
           
       # pour une variable
           # Modifier
           # test si c'est numeric
           if(is.numeric(data[[vardsc]]) || is.integer(data[[vardsc]])){
               # test s'il y a une aoutcom======
               if(is.null(outcom)){
                   # construction label
                   if(is.null(data)){
                       labl<-paste(substitute(vardsc))
                       vardsc<-vardsc
                   }else{
                       labl=vardsc
                       vardsc=data[[vardsc]]
                   }
                   vardsc1<-vardsc[!is.na(vardsc)];nob<-length(vardsc1)
                   vecmd<-ifelse(is.na(vardsc),1,0)
                   md<-sum(ifelse(is.na(vardsc),1,0))
                   pmd<-100*md/length(vecmd)
                   # test de la normalité asympotique
                   if(length(vardsc1)>5000){
                          pp<-round(shapiro.test(vv)$p,digits=4)
                   }else{
                           pp<-round(shapiro.test(vardsc1)$p,digits=4)
                   }
                   rs<-c(round(c(mean(vardsc1),sd(vardsc1),quantile(vardsc1,prob=0.5),quantile(vardsc1,prob=0.75)-quantile(vardsc1,prob=0.25),min(vardsc1),max(vardsc1),pmd,nob),digits=pre),pp)
                   Rs<-t(rs)
                   colnames(Rs)<-c("Mean","SD","Median","IQR","Min","Max","Md(%)","N","P-value")
                   rownames(Rs)<-labl
               }else{
                   if(test==1){
                       mod<-kruskal.test(formula(paste(vardsc,"~",outcom)),data=data)
                   }else{
                       mod<-kruskal.test(formula(paste(outcom,"~",vardsc)),data=data)
                   }
                   if(mod$p.value<0.001){pp="<0.001"}else{pp=paste(round(mod$p.value,digits=3))}
                   labl<-NULL
                   vm<-NULL
                   ss<-unique(data[[outcom]])
                   for(ux in ss){
                       labl<-c(labl,c("N",paste("M(",outcom,"=",ux,")",sep="")))
                       vv<-data[data[[outcom]]==ux,][[vardsc]]
                       vv<-vv[!is.na(vv)]
                       vm<-c(vm,c(length(vv),round(mean(vv),digits=pre)))
                   }
                   Rs<-t(c(vm,pp))
                   colnames(Rs)<-c(labl,"p-value")
                   rownames(Rs)<-vardsc
               }#===============
               return(Rs)
           }else{
               stop("The variable is not numeric or integer")
           }
       }
       
       

