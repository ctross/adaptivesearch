######################################################################## Non-Destructive
###################################################################### Mean Hits
  df <- data.frame(Adaptive=Res.d.H[,3],Lévy=Res.d.h[,3])
  dfs <- stack(df)
  colnames(dfs)<-c("Mean_Encounters","Search_Mode")
  ggplot(dfs, aes(x=Mean_Encounters)) + geom_density(aes(group=Search_Mode, colour=Search_Mode, fill=Search_Mode), alpha=0.5)  +theme(axis.text=element_text(size=14),        axis.title=element_text(size=16,face="bold")) 
   ggsave("ND-M-E_Torus.pdf",height=6, width=8)
   
###################################################################### SD Hits
  df <- data.frame(Adaptive=c(Res.d.H[,6]/Res.d.H[,3]),Lévy=c(Res.d.h[,6]/Res.d.h[,3]))
  dfs <- stack(df)
  colnames(dfs)<-c("CV_Encounters","Search_Mode")
  ggplot(dfs, aes(x=CV_Encounters)) + geom_density(aes(group=Search_Mode, colour=Search_Mode, fill=Search_Mode), alpha=0.5)  +theme(axis.text=element_text(size=14),        axis.title=element_text(size=16,face="bold"))  
     ggsave("ND-M-SD_Torus.pdf",height=6, width=8)

###################################################################### Destructive
###################################################################### Mean Hits
  df <- data.frame(Adaptive=Res.D.H[,3],Lévy=Res.D.h[,3])
  dfs <- stack(df)
  colnames(dfs)<-c("Mean_Encounters","Search_Mode")
  ggplot(dfs, aes(x=Mean_Encounters)) + geom_density(aes(group=Search_Mode, colour=Search_Mode, fill=Search_Mode), alpha=0.5)  +theme(axis.text=element_text(size=14),        axis.title=element_text(size=16,face="bold")) 
   ggsave("D-M-E_Torus.pdf",height=6, width=8)
   
###################################################################### SD Hits
  df <- data.frame(Adaptive=c(Res.D.H[,6]/Res.D.H[,3]),Lévy=c(Res.D.h[,6]/Res.D.h[,3]))
  dfs <- stack(df)
  colnames(dfs)<-c("CV_Encounters","Search_Mode")
  ggplot(dfs, aes(x=CV_Encounters)) + geom_density(aes(group=Search_Mode, colour=Search_Mode, fill=Search_Mode), alpha=0.5)  +theme(axis.text=element_text(size=14),        axis.title=element_text(size=16,face="bold")) 
     ggsave("D-M-SD_Torus.pdf",height=6, width=8)
   
#################################################################### T tests     
  print( t.test(Res.d.H[,3],Res.d.h[,3]) )
  print( t.test(Res.d.H[,3],Res.d.h[,3])$estimate[1]-
         t.test(Res.d.H[,3],Res.d.h[,3])$estimate[2])
   
  print( t.test(Res.D.H[,3],Res.D.h[,3]) )
  print( t.test(Res.D.H[,3],Res.D.h[,3])$estimate[1]-
         t.test(Res.D.H[,3],Res.D.h[,3])$estimate[2])