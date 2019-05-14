#Equivalent Circuit Data March 15 2019
install.packages("plot3D")
library("plot3D")
library ("plotly")

#Definitions Electrolyte Resistance (Rs), charge transfer resistance (Rcp)
# Constant phase element (CPE): CPE-T is pseudo-capacitance which is Q;
#CPE-P is related to semi-circle of Nyquist plot (normally notation 'n')
#calculate true capacitance (R^(1-n)/n)*Q^(1/n).

Electrolyte_Resistance <- c(39,
                            39,
                            45,
                            59,
                            59.91,
                            75.61,
                            77.3,
                            79.8,
                            89.1,
                            83.54,
                            38.46,
                            38.88,
                            40.27,
                            42.22,
                            7849,
                            7947,
                            7273,
                            7624,
                            7461,
                            7283,
                            58.22,
                            59.19,
                            61.63,
                            62.39,
                            64.26,
                            67.9,
                            44.7,
                            44.9,
                            45.9,
                            49.7,
                            51.7,
                            54.7)

Pseudo_capacitance <- c(2.81e-6,
                        3.19e-6,
                        3.73e-6,
                        1.23e-6,
                        1.186e-6,
                        2.19e-6,
                        2.18e-6,
                        1.96e-6,
                        1.68e-6,
                        1.73e-6,
                        6.28e-6,
                        7.16e-6,
                        3.60e-6,
                        4.32e-6,
                        4.16e-6,
                        3.35e-6,
                        2.88e-6,
                        2.88e-6,
                        1.63e-6,
                        2.06e-6,
                        4.96e-6,
                        4.12e-6,
                        4.19e-6,
                        4.42e-6,
                        3.46e-6,
                        3.26e-6,
                        4.00e-6,
                        2.27e-6,
                        1.91e-6,
                        1.56e-6,
                        1.80e-6,
                        1.6e-6)

Charge_transfer_resistance <-c(0.522e6,
                               1.054e6,
                               2.31e6,
                               2.21e6,
                               1.21e6,
                               2.28e6,
                               2.70e6,
                               1.78e6,
                               1.01e6,
                               1.11e6,
                               168000,
                               24300,
                               41820,
                               51700,
                               5.09e5,
                               13.3e5,
                               5.58e5,
                               9.88e5,
                               9.11e5,
                               9.68e5,
                               1.14e12,
                               5.65e12,
                               5.54e12,
                               1.13e12,
                               6.14e12,
                               0.95e12,
                               2.00e6,
                               4.00e6,
                               2.37e6,
                               4.00e6,
                               4.00e6,
                               2.00e6)

Analyte=rep(c("Methanol","Ethanol","Acetone","H2S","IPA","Acetonitrile"),times=c(5,5,4,6,6,6))


##create dataframe
analytedata<-data.frame(Electrolyte_Resistance,Pseudo_capacitance,Charge_transfer_resistance,Analyte)
analytedata$Analyte<-as.factor(analytedata$Analyte)

#remove H2S from dataframe rows 15-20 
analytedatanoH2S<-analytedata[-c(15,16,17,18,19,20),,drop=F]

##3D plot -didn't work
scatter3D(analytedata$Electrolyte_Resistance,analytedata$Pseudo_capacitance,analytedata$Charge_transfer_resistance,bty = 'g', col.var = analytedata$Analyte, main="0.5M KCl Solvent", xlab="Rs",ylab="CPE-T",zlab="Rcp",phi=0, colkey=FALSE, ticktype="simple")

##plotly3D - used 
plot2 <- plot_ly(analytedata,x=Electrolyte_Resistance, y=Pseudo_capacitance,z= Charge_transfer_resistance, color=Analyte, type="scatter3d", mode="markers")
plot2

p<-plot_ly(analytedata,x=Electrolyte_Resistance, y=Pseudo_capacitance,z= Charge_transfer_resistance, color=Analyte, type="scatter3d", mode="markers")%>%
  layout(scene = list(xaxis = list(title = 'Rs'),
                      yaxis = list(title = 'CPE-T'),
                      zaxis = list(title = 'Rcp')))
p

p2<-plot_ly(analytedatanoH2S,x=analytedatanoH2S$Electrolyte_Resistance, y=analytedatanoH2S$Pseudo_capacitance,z= analytedatanoH2S$Charge_transfer_resistance, color=analytedatanoH2S$Analyte, type="scatter3d", mode="markers")%>%
  layout(scene = list(xaxis = list(title = 'Rs'),
                      yaxis = list(title = 'CPE-T'),
                      zaxis = list(title = 'Rcp')))
p2

#2D plots

twodplot1<-ggplot(analytedata, aes(x=Pseudo_capacitance, y=Electrolyte_Resistance,group=Analyte, color=Analyte))+geom_point()
twodplot1<-twodplot1+stat_ellipse() + xlab("Pseudo-capacitance (CPE-T)")+ ylab("Electrolyte Resistance (Rs)")+theme_minimal()
twodplot1

twodplot2<-ggplot(analytedatanoH2S, aes(x=Pseudo_capacitance, y=Electrolyte_Resistance,group=Analyte, color=Analyte))+geom_point()
twodplot2<-twodplot2+stat_ellipse() + xlab("Pseudo-capacitance (CPE-T)")+ ylab("Electrolyte Resistance (Rs)")+theme_minimal()
twodplot2

multiplot(twodplot1,twodplot2, cols=2)
pdffile <- "Equivalentdata.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
twodplot1
twodplot2
dev.off()

##share chart
Sys.setenv("plotly_username"="nishattasnim")
Sys.setenv("plotly_api_key"="Y9JHw6MATu9HmSsDLrDo")
api_create(p, filename = "r-KCl_3D")
api_create(p2, filename = "r-KCl_3D_noH2S")


##export dataframe
write.csv(analytedata,'KCl3Ddata.csv', row.names = FALSE)


