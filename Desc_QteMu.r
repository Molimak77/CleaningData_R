Desc_QteMu<-function(Vvardsc,vtest=NULL,data=NULL,outcom=NULL,pre=2){
    # la fonction qui fait la descrition multi-varié des continue
           #Vvardsc : vecteur des variable
           if(is.null(vtest)){
             vecT=rep(1,length(Vvardsc))
           }else{
            vecT=vtest
           }
           DatR<-NULL;i=1
           for(ux in Vvardsc){
               #print(ux)
            DatR<-rbind(DatR,Desc_QteUn(ux,data=data,outcom=outcom,pre=pre,test=vecT[i]))
            i=i+1
           }
           return(DatR)
       }
       

