tri_data<-function(data,ID,vecex,vacodm1=NA,vecPm=NULL,gpf=TRUE,dgt2=1){
    # la fonction qui fait le tri à plat des données
    # data : les données
    # ID : l'identifiant
    # vecex : les variables à exclure au traitement
    # vacodm : le vecteur des codes de données mananquante
    # vecPm : vecteur facteur mutiplicatif pour des donnes aberrantes
    # Construction des variable quali et quant
    # gpf: pour indiquer si nous voulons des graph avec valeur par defaut TRUE (graphe)
    # dgt2 : la numbre des chiffres après la virgules
    ll0<-SepQlCo_vaa(data,c(ID,vecex))
    
    # traitement des variable qualitative
    vafact<-ll0$va_qual
    Rsf<-tri_mvafact(vafact,ID,data,vfacMo=vecPm,re=gpf)
    # traitement des variable continu e
    vacont<-ll0$va_cont
    Rsc<-tri_mvacont(vacont,ID,data,vecdm=vacodm1,re=gpf,dgt1=dgt2)
return(list(Rsvaf=Rsf,Rsvaco=Rsc))
}
