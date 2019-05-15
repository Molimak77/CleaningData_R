tri_mvacont<-function(vecvaa,Id,data,vecdm=NA,re=TRUE,dgt1=1){
    # vecvaa: vecteur des noms des variables aléatoir continue
    # Id : identifiant
    # vecdm : vecteur des dm
    
    matR<-NULL
    listR<-list()
    lstfc<-list()
    i=1
    if(re==FALSE){
    for(uu in vecvaa){
        ll<-tri_uvacont(uu,Id,data,dm=vecdm[i],dgt=dgt1)
        #print(ll)
        matR<-rbind(matR,ll$matD)
        listR[[uu]]<-ll$lstIdv
        #lstfc[[uu]]<-ll$fcthist
        i=i+1
        #print(uu)
    }
    }else{
        for(uu in vecvaa){
            #print(uu)
            ll<-tri_uvacont(uu,Id,data,dm=vecdm[i],dgt=dgt1)
            matR<-rbind(matR,ll$matD)
            listR[[uu]]<-ll$lstIdv
            lstfc[[uu]]<-ll$fcthist
            i=i+1
        }
    }
    names(listR)<-vecvaa
    names(lstfc)<-vecvaa
    return(list(matR=matR,listR=listR,lstfc=lstfc))
}
