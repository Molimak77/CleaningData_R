SepQlCo_vaa<-function(data,va.exc){
    # La fonction qui permet de separer les variables qualitative des continues et des dates
    # revoit trois vecteur var quali, conti et date 
    #data : la data.frame
    #va.exc : les variables à exclure
    vec<-names(data)
    if(missing(va.exc)){
        vec1<-vec
    }else{vec1<-vec[vec%in%va.exc!=TRUE]}
    vecqual<-NULL
    veccont<-NULL
    vecdat<-NULL
    vecknow<-NULL
    veccha<-NULL
    for(ux in vec1){
        if(class(data[[ux]])=="factor"){
            vecqual<-c(vecqual,ux)
        }else{
            if(class(data[[ux]])=="numeric"|class(data[[ux]])=="integer"){
                veccont<-c(veccont,ux)
            }else{
                 if(class(data[[ux]])=="Date"){
                     vecdat<-c(vecdat,ux)
                 }else{
                     if(class(data[[ux]])=="character"){
                         veccha<-c(veccha,ux)
                     }else{
                        vecknow<-c(vecknow,ux)
                     }
                 }
            }
        }
    }
    rs<-t(c(paste(length(vecqual)),paste(length(veccont)),paste(length(vecdat)),paste(length(veccha)),paste(length(vecknow))))
    dat<-data.frame(rs)
    names(dat)<-c("Nbr continue va","Nbr qualitative va","Nbr date va","Nbr Character va","Nbr Inc va")
    
    if(length(veccha)==0){
     return(list(desct=dat,va_qual=vecqual,va_cont=veccont,va_date=vecdat,va_Char=veccha,va_indf=vecknow))
    }else{
        print(paste("Il y a ",paste(length(veccha))," variable(s) character(s) il faut absolument changer ces variables en facteur pour l'analyse \n"))
        print(paste("Voici le ou les dite(s) variables : ",paste(veccha,",")," à changer"))
       return(list(desct=dat,va_qual=vecqual,va_cont=veccont,va_date=vecdat,va_Char=veccha,va_indf=vecknow))
    }
    
}
