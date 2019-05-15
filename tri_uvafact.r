tri_uvafact<-function(vaa,Id,data,facMo=NULL){
    # la fonction qui permet de faire le trie d'une variable qualitative
    # vaa : variable aleatoir qualitative
    # Id ; identifiant
    # data : data
    # facMo : facteur multiplucative pour les valeurs aberrante n/facMo*length(echa)
    
    #Etape decrire toutes les catégorie
    va<-data[[vaa]]
    tt<-table(va,exclude=FALSE)
    nam<-labels(tt)$va
    tt1<-as.vector(tt)
    pourc<-round(100*tt1/length(va),digits=1)
    if(is.null(facMo)){
        valmodra<-round(100/(3*length(nam)),digits=2)
    }else{
        valmodra<-round(100/(facMo*length(nam)),digits=2)
    }
    # la ou les modalité rare
    vecmodra<-nam[which(pourc<=valmodra)]
    IndModAb<-as.vector(data[which(va%in%vecmodra),][[Id]])
    if(length(IndModAb)==0){IndModAb="Neant"}else{IndModAb<-IndModAb}
    vedd<-ifelse(pourc<=valmodra,"Yes","No")
    vec<-c(vaa,rep("",length(nam)-1))
    nam1<-nam
    tt2<-tt1
    anop<-rep(valmodra,length(nam))
    pourc1<-pourc
    # organisation des resultats
    datR<-data.frame(vec,nam1,tt2,pourc1,vedd,anop)
    colnames(datR)<-c("Variable","Levels","N","(%)",paste("Test_Mod"),"%_Freq_lim")
    
    # le graphique
    #graph<-function(){
    #barplot(pourc,cex.main=0.9,main=vaa,
    #xlab=vaa,
    #ylab="Pourcentage",
    #border="blue",
    #col="grey",
    #   density=100)
    #}
    Mak<-as.vector(labels(table(va))$va)
    Percentage<-as.vector(round(100*table(va)/length(va),digits=1))
    datf<-data.frame(Percentage,Mak)
    #names(datf)<-c("Percentage",vaa)
    
    my_barplot<-function(mycol="steelblue"){
                ggplot(data=datf,aes(x=Mak,y=Percentage))+
                geom_bar(stat="identity",fill=mycol)+
                geom_text(aes(label=Percentage),vjust=-0.3,size=3.5)+
                labs(title=paste("Distribution of ",vaa,sep=""),x=vaa,y="Rate(%)")+
                theme_minimal()
    }
    return(list(datR=datR,grah=my_barplot,lstId=IndModAb))
}
