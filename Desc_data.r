Desc_data<-function(data1,vecex,outcom1=NULL,vtest1=NULL,dgt2=2){
    # la fonction qui fait la description des données
    # data : les données
    # vecex : les variables à exclure au traitement
    # outcom : la variable d'intêret
    # Construction des variables qualitatifs et quant
    ll0<-SepQlCo_vaa(data1,vecex)
    
    # les variable
    vafact<-ll0$va_qual
    vacont<-ll0$va_cont
    # distribution sans outcom
    if(is.null(outcom1)){
          llf1<-Desc_fctMu(vafact,data1,prc=dgt2)
          llc1<-Desc_QteMu(vacont,data=data1,pre=dgt2)
    }else{
          # vendd
          if(length(vtest1)>=length(vacont)){
              vtest2=vtest1
          }else{
             vtest2=c(vtest1,rep(vtest1[length(vtest1)],length(vacont)-length(vtest1)))
          }
          llf1<-Desc_fctMu(vafact,data1,outcom1=outcom1,prc=dgt2)
          llc1<-Desc_QteMu(vacont,vtest=vtest2,data=data1,outcom=outcom1,pre=dgt2)
    }
    # Traitement des variables continues
RstaF=llf1$tabR
RstaC=llc1
return(list(Rdatf=RstaF,RdatC=llc1))
}
