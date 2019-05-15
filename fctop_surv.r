fctop_surv<-function(tim,event,va.co,leave = 20){
    # tim ; le temps
    # event : les variable evement
    #Etape suppression des données manquante
    datt=data.frame(tim,event,va.co)
    datt1=na.omit(datt)
    tim1=datt1$tim
    event1=datt1$event
    va.co1=datt1$va.co
    # estimateurf
    n=length(va.co1)
    score=sort(va.co1)
    chisq.stat =rep(0,n)
    # correction de borne forni
    kk<-length(c((leave + 1):(n - leave + 1)))
    # la boucle de calcul 
    for (i in (leave + 1):(n - leave + 1)) {
        group=ifelse(va.co1>=score[i],1,0)
        res = survdiff(Surv(tim1,event1) ~ group)
        chisq.stat[i] = res$chisq
    }
    cutoff = score[which.max(chisq.stat)]
    pvacc=0.05/kk
  return(list(cutoff=cutoff,pvc=pvacc))
}
