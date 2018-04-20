#Designate patients with no local GP codes, only hospital codes and/or outside GP codes

home<-crea.rep[!crea.rep$Source=="salfordt" & crea.rep$CCG=="NHSSalfordCCG",]
unique(crea.rep$PatientID[crea.rep$PatientID %in% home$PatientID])
crea.rep$ReferredIn<-ifelse(crea.rep$PatientID %in% home$PatientID,0,1)

length(unique(crea.rep$PatientID[crea.rep$ReferredIn==1]))
save(crea.rep,file="crea.repongoing.rda")
