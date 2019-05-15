Sup_vaa<-function(data,vaa){
    # version v.01
    # cette fonction permet de supprimer des variables d'une data.frame
    # vaa : the variable aleatoir
    # data : la base de donnée
    for(xx in vaa){
        data[[xx]]<-NULL
    }
    return(data)
}
