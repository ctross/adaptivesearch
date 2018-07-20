########################################################################## Plots
################################################################ Non-Destructive
###################################################################### Step Size
 df <- data.frame(Mean_Log_Stepsize=c(Res.d.h[,1],Res.d.H[,1],Res.d.B[,1]),  SD_Log_Stepsize=c(Res.d.h[,4],Res.d.H[,4],Res.d.B[,4]), 
                  Search_Mode=c(rep("Lévy",length(c(Res.d.h[,4]))),rep("Adaptive",length(c(Res.d.H[,4]))),rep("Brownian",length(c(Res.d.H[,4])))))
 qplot(Mean_Log_Stepsize, SD_Log_Stepsize, data = df, colour = Search_Mode)  +theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))
         ggsave("ND-S_Full.pdf",height=6, width=8)
         
###################################################################### Heading 
 df <- data.frame(Mean_Heading=c(Res.d.h[,2],Res.d.H[,2],Res.d.B[,2]),  D_Heading=c(Res.d.h[,5],Res.d.H[,5],Res.d.B[,5]), 
                  Search_Mode=c(rep("Lévy",length(c(Res.d.h[,4]))),rep("Adaptive",length(c(Res.d.H[,4]))),rep("Brownian",length(c(Res.d.H[,4])))))
 qplot(Mean_Heading, D_Heading, data = df, colour = Search_Mode) +theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))
       ggsave("ND-H_Full.pdf",height=6, width=8)



################################################################### Destructive
###################################################################### Step Size
 df <- data.frame(Mean_Log_Stepsize=c(Res.D.h[,1],Res.D.H[,1],Res.D.B[,1]),  SD_Log_Stepsize=c(Res.D.h[,4],Res.D.H[,4],Res.D.B[,4]), 
                  Search_Mode=c(rep("Lévy",length(c(Res.d.h[,4]))),rep("Adaptive",length(c(Res.d.H[,4]))),rep("Brownian",length(c(Res.d.H[,4])))))
 qplot(Mean_Log_Stepsize, SD_Log_Stepsize, data = df, colour = Search_Mode)  +theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))
         ggsave("D-S_Full.pdf",height=6, width=8)
################################################################### Heading Size
 df <- data.frame(Mean_Heading=c(Res.D.h[,2],Res.D.H[,2],Res.D.B[,2]),  D_Heading=c(Res.D.h[,5],Res.D.H[,5],Res.D.B[,5]), 
                  Search_Mode=c(rep("Lévy",length(c(Res.d.h[,4]))),rep("Adaptive",length(c(Res.d.H[,4]))),rep("Brownian",length(c(Res.d.H[,4])))))
 qplot(Mean_Heading, D_Heading, data = df, colour = Search_Mode) +theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))
       ggsave("D-H_Full.pdf",height=6, width=8)


       