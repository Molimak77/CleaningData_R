tri_mvafact<-function(vecvar,Id,data,vfacMo=NULL,re=FALSE){
    # la fonction qui fait le tri de plusieurs variable qualitatives
    listI=list()
    marR=NULL
    graphl<-list()
    i=1
    if(re==FALSE){
    for(ux in vecvar){
        ll<-tri_uvafact(ux,Id,data,facMo=vfacMo[i])
        graphl[[ux]]<-ll$grah
        marR<-rbind(marR,ll$datR)
        listI[[ux]]<-ll$lstId
        i=i+1
    }
    }else{
        for(ux in vecvar){
            print(ux)
            ll<-tri_uvafact(ux,Id,data,facMo=vfacMo[i])
            marR<-rbind(marR,ll$datR)
            listI[[ux]]<-ll$lstId
            i=i+1
        }
    }
    return(list(marR=marR,graphl=graphl,listI=listI))
}
