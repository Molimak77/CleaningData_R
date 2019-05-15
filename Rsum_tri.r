Rsum_tri<-function(Rs01,x,vecnam=NULL){
    # 1 pour les bases de donne
    # 2 graphique
    # 3 liste des individu aberrants conti mineur et majeur
    
    # vecnam : nom de la variable
    
    if(x==1){rst<-list(tabfact=Rs01$Rsvaf[["marR"]],tabcont=Rs01$Rsvaco[["matR"]])}
    if(x==2){
        if(is.null(vecnam)){
            stop("Il manque le nom de la variable ")
        }else{
            rst0<-c(Rs01$Rsvaf[["graphl"]][[vecnam]],Rs01$Rsvaco[["lstfc"]][[vecnam]])
            rst<-rst0[[1]]
        }
    }
    if(x==3){
        if(is.null(vecnam)){
            stop("Il manque le nom de la variable ")
        }else{
            rst<-Rs01$Rsvaco[["listR"]][[vecnam]]
        }
    }
    return(rst)
}
