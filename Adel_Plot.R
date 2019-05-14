##Equivalent Circuit Data

Electrolyte_Resistance <- c( 53.98,
                             33.98,
                             45,
                             51,
                             52.98,
                             52,
                             1,
                             777,
                             0.01,
                             1)

Charge_Transfer_Resistance <- c( 235540,
                                 155360,
                                 155360,
                                 175360,
                                 182530,
                                 175360,
                                 217000,
                                 97487,
                                 60988,
                                 49400)
##used CPE1-T for H2O dataset
Pseudo_Capacitance <- c( 2.17E-6,
                         2.726E-6,
                         1.826E-6,
                         2.116E-6,
                         1.596E-6,
                         1.568E-6,
                         3.7E-11,
                         3.39E-11,
                         4.83E-11,
                         3.2E-11)
##used CPE1-P for H2O dataset
semicircle<- c( 0.879,
                0.96933,
                0.969,
                0.92593,
                0.92933,
                0.90918,
                0.98,
                0.997,
                0.964,
                0.99)


Solvent= rep(c("KCl","H2O"),times=c(6,4))

equivalentcircuit<-data.frame(Charge_Transfer_Resistance,Electrolyte_Resistance,Solvent)
equivalentcircuit$Solvent<-as.factor(equivalentcircuit$Solvent)

equivalentcircuit2<-data.frame(Charge_Transfer_Resistance,Electrolyte_Resistance,Pseudo_Capacitance,Solvent)
equivalentcircuit2$Solvent<-as.factor(equivalentcircuit2$Solvent)

equivalentcircuit3<-data.frame(Charge_Transfer_Resistance,Electrolyte_Resistance,Pseudo_Capacitance,semicircle,Solvent)
equivalentcircuit3$Solvent<-as.factor(equivalentcircuit3$Solvent)

pdffile <- "Adel_equivalentplots_H2S.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=5, height=3)

plot1<-ggplot(equivalentcircuit, aes(x=Charge_Transfer_Resistance, y=Electrolyte_Resistance,group=Solvent, color=Solvent))+geom_point()
plot1<-plot1+stat_ellipse() + ylab("Electrolyte resistance (Rs)")+ xlab("Charge transfer resistance (Rct)") +theme_minimal()

plot2<-ggplot(equivalentcircuit2, aes(x=Charge_Transfer_Resistance, y=Pseudo_Capacitance,group=Solvent, color=Solvent))+geom_point()
plot2<-plot2+stat_ellipse() + xlab("Charge Transfer Resistance (Rct)")+ ylab("Pseudo-capacitance (Q)")+theme_minimal()

plot3<-ggplot(equivalentcircuit2, aes(x=Electrolyte_Resistance, y=Pseudo_Capacitance,group=Solvent, color=Solvent))+geom_point()
plot3<-plot3+stat_ellipse() + xlab("Electrolyte resistance (Rs)")+ ylab("Pseudo-capacitance (Q)")+theme_minimal()

plot4<-ggplot(equivalentcircuit3, aes(x=Electrolyte_Resistance, y=semicircle,group=Solvent, color=Solvent))+geom_point()
plot4<-plot4+stat_ellipse() + xlab("Electrolyte resistance (Rs)")+ ylab("CPE-P")+theme_minimal()

plot5<-ggplot(equivalentcircuit3, aes(x=Charge_Transfer_Resistance, y=semicircle,group=Solvent, color=Solvent))+geom_point()
plot5<-plot5+stat_ellipse() + xlab("Charge Transfer Resistance (Rct)")+ ylab("CPE-P")+theme_minimal()+theme(text=element_text(size=16))

plot6<-ggplot(equivalentcircuit3, aes(x=Pseudo_Capacitance, y=semicircle,group=Solvent, color=Solvent))+geom_point()
plot6<-plot6+stat_ellipse() + xlab("Pseudo-capacitance (Q)")+ ylab("CPE-P")+theme_minimal()


multiplot(plot1,plot2,plot3,plot4,plot5,plot6,cols=2)
dev.off()

pdffile <- "Adel_H2S.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
plot5
dev.off()

######sensor response graphs######

##only using sensor 1 response##
##Frequency 1 H2O

##H2O plot 
conc <- c(0,
          0.0025,
          0.0075,
          0.0125,
          0.025,
          0.0375,
          0.0625,
          0.0875,
          0.0025,
          0.0075,
          0.0125,
          0.025,
          0.0375,
          0.0625,
          0.0875,
          0,
          0.0025,
          0.0075,
          0.0125,
          0.025,
          0.0625,
          0.0875,
          0.0025,
          0.0075,
          0.0125,
          0.025,
          0.0375,
          0.0625,
          0.0875)
  
impedence <- c(0,
               0.101577689,
               0.131587731,
               0.505658801,
               0.706746238,
               0.791568068,
               0.89989257,
               1,
               1,
               0.034824333,
               0,
               0.413050707,
               0.549097022,
               0.242993686,
               0.511881344,
               0,
               0.468267369,
               0.586352784,
               0.69690863,
               0.894142693,
               1,
               0.946441429,
               1,
               0.858627705,
               0.783551809,
               0.429431452,
               0.343567064,
               0.319930488,
               0)


solvent<-rep("H2O",times=(29))

analyte<-rep(c("Ethanol","IPA","Acetone","H2S"),times=c(8,7,7,7))

f1data<-data.frame(conc,impedence,solvent,analyte)
f1data$solvent<-as.factor(f1data$solvent)

##H2O plot
f1plot<-ggplot(f1data, aes(x=conc, y=impedence,group=analyte, color=analyte))+geom_point()
f1plot1<-f1plot+geom_line()+ ylab("Normalized impedence response")+ xlab("Concentration (%w/w)") +theme_minimal()+theme(text=element_text(size=16))
f1plot1

##KCL plot

concKCL<-c(0,
           0.0075,
           0.0125,
           0.025,
           0.0625,
           0.0875,
           0,
           0.0025,
           0.0075,
           0.0125,
           0.025,
           0.0375,
           0.0625,
           0.0875,
           0.0025,
           0.0075,
           0.0375,
           0.0625,
           0.0875,
           0,
           0.0025,
           0.0075,
           0.0125,
           0.025,
           0.0375,
           0.0625)

impedenceKCL <- c(0,
                  0.482049341,
                  0.580438564,
                  0.754923148,
                  1,
                  0.864658052,
                  0,
                  0.051193895,
                  0.244326339,
                  0.419984182,
                  0.471043308,
                  0.479499453,
                  0.665790903,
                  1,
                  1,
                  0.358530768,
                  0.332320419,
                  0.491468175,
                  0,
                  0,
                  0.377217173,
                  0.480047648,
                  0.639513404,
                  0.642389395,
                  1,
                  0.824724274)

solventtype<-rep("KCl",times=(26))
analytetype<-rep(c("Ethanol","IPA","Acetone","H2S"),times=c(6,8,5,7))

f1KCLdata<-data.frame(concKCL,impedenceKCL,solventtype,analytetype)
f1plotKCL<-ggplot(f1KCLdata, aes(x=concKCL, y=impedenceKCL,group=analytetype, color=analytetype))+geom_point()
f1plot2<-f1plotKCL+geom_line()+ ylab("Normalized impedence response")+ xlab("Concentration (%w/w)") +theme_minimal()+theme(text=element_text(size=16))
f1plot2

multiplot(f1plot1,f1plot2, cols=2)

pdffile <- "Solvent_Analyte2.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
f1plot2
dev.off()

##make 3D plot with plotly
#can't change color parameter. Downgrade: pip uninstall plotly pip install plotly==2.7.0

library (plotly)
p <- plot_ly(equivalentcircuit3,x=Charge_Transfer_Resistance, y= Pseudo_Capacitance,z= semicircle, color=Solvent, type="scatter3d", mode="markers")
p

p2 <- plot_ly(equivalentcircuit3,x=Electrolyte_Resistance, y= Pseudo_Capacitance,z= semicircle, color=Solvent, type="scatter3d", mode="markers")
p2




