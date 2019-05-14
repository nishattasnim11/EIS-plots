library (ggplot2) #version 3.1.0
library(gridExtra)
library (dplyr)

###functions used###
#1
library(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
#2
function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###start of data analysis###

##Solvent=H2O
#frequency=1 Analyte= Ethanol, Acetone, AceNT, H2S 
Impedance<- c(0.101577689,
              0.611689754,
              0,
              0.505658801,
              0.664947273,
              0.101145538,
              0.706746238,
              0.705350406,
              0.360824994,
              0.791568068,
              0.91330496,
              0.689692714,
              0.89989257,
              0.994353342,
              0.793422595,
              1,
              1,
              1,
              
              0,
              0,
              0,
              0.468267369,
              0.102220367,
              0.102327061,
              0.586352784,
              0.373139192,
              0.174065707,
              0.69690863,
              0.440869482,
              0.241175434,
              0.894142693,
              0.594092218,
              0.423747775,
              1,
              1,
              1,
              0.946441429,
              0.634914017,
              0.970120165,
              
              0,
              0,
              0.10274829,
              0.019805773,
              0.157188444,
              0.07174976,
              0.331034896,
              0.079552153,
              0.448720107,
              0.410867877,
              0.585180863,
              0.502938264,
              0.896891344,
              0.688498593,
              1,
              1,
              
              1,
              1,
              0.858627705,
              0.995115742,
              0.783551809,
              0.571571097,
              0.429431452,
              0.347798833,
              0.166839148,
              0.343567064,
              0.226543824,
              0.319930488,
              0)

concentration<-c(0.0025,
                 0.0025,
                 0.0025,
                 0.0125,
                 0.0125,
                 0.0125,
                 0.025,
                 0.025,
                 0.025,
                 0.0375,
                 0.0375,
                 0.0375,
                 0.0625,
                 0.0625,
                 0.0625,
                 0.0875,
                 0.0875,
                 0.0875,
                 
                 0,
                 0,
                 0,
                 0.0025,
                 0.0025,
                 0.0025,
                 0.0075,
                 0.0075,
                 0.0075,
                 0.0125,
                 0.0125,
                 0.0125,
                 0.025,
                 0.025,
                 0.025,
                 0.0625,
                 0.0625,
                 0.0625,
                 0.0875,
                 0.0875,
                 0.0875,
                 
                 0,
                 0,
                 0.0025,
                 0.0025,
                 0.0075,
                 0.0075,
                 0.0125,
                 0.0125,
                 0.025,
                 0.025,
                 0.0375,
                 0.0375,
                 0.0625,
                 0.0625,
                 0.0875,
                 0.0875,
                 
                 0.0025,
                 0.0025,
                 0.0075,
                 0.0075,
                 0.0125,
                 0.0125,
                 0.025,
                 0.025,
                 0.025,
                 0.0375,
                 0.0375,
                 0.0625,
                 0.0625)

Analyte=rep(c("Ethanol","Acetone","Acetonitrile","H2S"),times=c(18,21,16,13))

h2odata<-data.frame(Impedance,concentration, Analyte)
h2odata$concentration<-as.factor(h2odata$concentration)
h2odata$Analyte<-as.factor(h2odata$Analyte)
print(levels(h2odata$concentration))
print(levels(h2odata$Analyte))
head(h2odata)

#use subset function for individual analyte plots, or frequency plots
ethanol<-subset(h2odata,Analyte=="Ethanol")


#frequency=1000 Analyte= Ethanol, Acetone, AceNT, H2S and IPA 
I1000<-c(0.181208056,
         0.531458889,
         0,
         0.508641351,
         0.640562973,
         0.490932666,
         0.554429656,
         0.747850325,
         0.718051399,
         0.585504006,
         0.785070565,
         0.72041232,
         0.828827231,
         0.856992562,
         0.846101771,
         1,
         1,
         1,
         
         1,
         0.824246573,
         0.034824333,
         0,
         1,
         0,
         0.413050707,
         0.080499469,
         0.549097022,
         0.109934497,
         0.806765654,
         0,
         0.242993686,
         0.511881344,
         0.008062045,
         0.005629514,
         
         0,
         0,
         0.043031621,
         0.011275188,
         0.059314211,
         0.019598052,
         0.127311474,
         0.110116521,
         0.585357106,
         0.454348842,
         1,
         1,
         0.974631829,
         0.914345818,
         
         0,
         0,
         0.23848617,
         0.237654358,
         0.353560211,
         0.408705684,
         0.596869512,
         0.512260168,
         0.616544014,
         0.534889606,
         0.635878069,
         0.639634786,
         0.907445451,
         0.802924426,
         1,
         1,
         
         1,
         1,
         0.459613943,
         0.429893642,
         0.423715968,
         0.267932249,
         0.451774104,
         0.258390099,
         0.277208734,
         0.40182741,
         0.007440115,
         0.121494797,
         0)

c1000<-c(0.0025,
         0.0025,
         0.0025,
         0.0125,
         0.0125,
         0.0125,
         0.025,
         0.025,
         0.025,
         0.0375,
         0.0375,
         0.0375,
         0.0625,
         0.0625,
         0.0625,
         0.0875,
         0.0875,
         0.0875,
         
         0,
         0,
         0.0075,
         0.0075,
         0.0125,
         0.0125,
         0.025,
         0.025,
         0.0375,
         0.0375,
         0.0375,
         0.0625,
         0.0625,
         0.0875,
         0.0875,
         0.0875,
         
         0,
         0,
         0.0025,
         0.0025,
         0.0075,
         0.0075,
         0.0125,
         0.0125,
         0.025,
         0.025,
         0.0625,
         0.0625,
         0.0875,
         0.0875,
         
         0,
         0,
         0.0025,
         0.0025,
         0.0075,
         0.0075,
         0.0125,
         0.0125,
         0.025,
         0.025,
         0.0375,
         0.0375,
         0.0625,
         0.0625,
         0.0875,
         0.0875,
         
         0.0025,
         0.0025,
         0.0075,
         0.0075,
         0.0125,
         0.0125,
         0.025,
         0.025,
         0.025,
         0.0375,
         0.0375,
         0.0625,
         0.0625)

Analyte1000=rep(c("Ethanol","IPA","Acetone","Acetonitrile","H2S"),times=c(18,16,14,16,13))

h2odata1000<-data.frame(I1000,c1000, Analyte1000)
h2odata1000$c1000<-as.factor(h2odata1000$c1000)
h2odata1000$Analyte1000<-as.factor(h2odata1000$Analyte1000)

##Solvent=KCL
#frequency=1 Analyte= Ethanol, IPA, Acetone, H2S 
ki<-c(0,
      0,
      0.482049341,
      0.038422342,
      0.580438564,
      0.625880341,
      0.754923148,
      0.677715819,
      1,
      1,
      
      0,
      0,
      0.051193895,
      0.099041183,
      0.244326339,
      0.275906157,
      0.419984182,
      0.646129903,
      0.471043308,
      0.649316323,
      0.665790903,
      0.825705271,
      1,
      1,
      
      1,
      0.092833295,
      0.332320419,
      1,
      0,
      0.96438428,
      
      0,
      0,
      0.377217173,
      0,
      0.480047648,
      0.560686985,
      0.026611032,
      0.639513404,
      0.781464772,
      0.065537035,
      0.642389395,
      1,
      0.350330535,
      1,
      0.636533841,
      1,
      0.824724274,
      0.600011424
      )

kc<-c(0,
      0,
      0.0075,
      0.0075,
      0.0125,
      0.0125,
      0.025,
      0.025,
      0.0625,
      0.0625,
      
      0,
      0,
      0.0025,
      0.0025,
      0.0075,
      0.0075,
      0.0125,
      0.0125,
      0.025,
      0.025,
      0.0625,
      0.0625,
      0.0875,
      0.0875,
      
      0.0025,
      0.0025,
      0.0375,
      0.0375,
      0.0875,
      0.0875,
      
      0,
      0,
      0.0025,
      0.0025,
      0.0075,
      0.0075,
      0.0075,
      0.0125,
      0.0125,
      0.0125,
      0.025,
      0.025,
      0.025,
      0.0375,
      0.0375,
      0.0375,
      0.0625,
      0.0625
      )

ka=rep(c("Ethanol","IPA","Acetone","H2S"),times=c(10,14,6,18))

kcldata<-data.frame(ki,kc, ka)
kcldata$kc<-as.factor(kcldata$kc)
kcldata$ka<-as.factor(kcldata$ka)

#frequency=1000 Analyte= Ethanol, IPA, Acetone,H2S 
ki1000<-c(0,
          0,
          0.576818479,
          0.065843434,
          0.669525949,
          0.332647943,
          0.900849161,
          0.684914592,
          1,
          1,
         
          0,
          0,
          0.059508972,
          0.097568742,
          0.211971209,
          0.30273891,
          0.385210399,
          0.69919498,
          0.451711312,
          0.681557357,
          0.698415583,
          0.895417508,
          1,
          1,
          
          0.086466646,
          0.062779454,
          0.83610673,
          0.269280245,
          0.877295979,
          0.687366425,
          1,
          1,
          0.202645094,
          0.469397747,
          
          0,
          0,
          0.039636529,
          0,
          0.784175958,
          1,
          0.006923797,
          0.788284879,
          0.62941353,
          0.034423475,
          0.831585177,
          0.455988295,
          0.090185318,
          1,
          0.58524061,
          1,
          0.890891103,
          0.424011956
          )

kc1000<-c(0,
          0,
          0.0075,
          0.0075,
          0.0125,
          0.0125,
          0.025,
          0.025,
          0.0625,
          0.0625,
          
          0,
          0,
          0.0025,
          0.0025,
          0.0075,
          0.0075,
          0.0125,
          0.0125,
          0.025,
          0.025,
          0.0625,
          0.0625,
          0.0875,
          0.0875,
          
          0.0025,
          0.0025,
          0.0125,
          0.0125,
          0.025,
          0.025,
          0.0375,
          0.0375,
          0.0875,
          0.0875,
          
          0,
          0,
          0.0025,
          0.0025,
          0.0075,
          0.0075,
          0.0075,
          0.0125,
          0.0125,
          0.0125,
          0.025,
          0.025,
          0.025,
          0.0375,
          0.0375,
          0.0375,
          0.0625,
          0.0625
          )

ka1000=rep(c("Ethanol","IPA","Acetone","H2S"),times=c(10,14,10,18))

kcldata1000<-data.frame(ki1000,kc1000, ka1000)
kcldata1000$kc1000<-as.factor(kcldata1000$kc1000)
kcldata1000$ka1000<-as.factor(kcldata1000$ka1000)

##get mean and sd
#H2O
h2odata2<-data_summary(h2odata, varname = "Impedance", groupnames = c("concentration","Analyte"))
h20data10002<-data_summary(h2odata1000, varname = "I1000", groupnames = c("c1000","Analyte1000"))
##subset out IPA
f1000edited<-subset(h20data10002,Analyte1000 =="Ethanol" | Analyte1000=="Acetone" | Analyte1000=="Acetonitrile"| Analyte1000=="H2S")

#KCl
kcldata2<-data_summary(kcldata, varname = "ki", groupnames = c("kc","ka"))
kcldata10002<-data_summary(kcldata1000, varname = "ki1000", groupnames = c("kc1000","ka1000"))


##plots
###with line

##H20 plots
f1H2O<-ggplot(h2odata2, aes(x=concentration,y=Impedance,group=Analyte, color=Analyte)) +
  geom_errorbar(aes(ymin=Impedance-sd,ymax=Impedance+sd),color="grey3",width=0.1)+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylab(expression("Impedance (Ω)"))+
  xlab("Concentration (% w/w)")+
  theme(text=element_text(size=16))+
  ggtitle("H2O solvent, f=1Hz")

print(f1H2O)

##without IPA so using subsetted data. For full data use h20data10002 data frame
f1000H2O<-ggplot(f1000edited, aes(x=c1000,y=I1000,group=Analyte1000, color=Analyte1000)) +
  geom_errorbar(aes(ymin=I1000-sd,ymax=I1000+sd),color="grey3",width=0.1)+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylab(expression("Impedance (Ω)"))+
  xlab("Concentration (% w/w)")+
  theme(text=element_text(size=16))+
  ggtitle("H2O solvent, f=1000Hz")+
  labs(color="Analyte")

print(f1000H2O)

##KCL plots
f1KCL<-ggplot(kcldata2, aes(x=kc,y=ki,group=ka, color=ka)) +
  geom_errorbar(aes(ymin=ki-sd,ymax=ki+sd),color="grey3",width=0.1)+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylab(expression("Impedance (Ω)"))+
  xlab("Concentration (% w/w)")+
  theme(text=element_text(size=16))+
  ggtitle("KCl solvent, f=1Hz")+
  labs(color="Analyte")

print(f1KCL)

f1000KCL<-ggplot(kcldata10002, aes(x=kc1000,y=ki1000,group=ka1000, color=ka1000)) +
  geom_errorbar(aes(ymin=ki1000-sd,ymax=ki1000+sd),color="grey3",width=0.1)+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylab(expression("Impedance (Ω)"))+
  xlab("Concentration (% w/w)")+
  theme(text=element_text(size=16))+
  ggtitle("KCl solvent, f=1000Hz")+
  labs(color="Analyte")

print(f1000KCL)

#print
pdffile <- "Adel_EIS_H2O.pdf"
pdf(paste(pdffile,sep=""), paper="letter", width=8, height=6)
f1H2O
f1000H2O
f1KCL
f1000KCL
dev.off()

#export csv
write.csv(h2odata,"Adel_EIS_h20.csv")
write.csv(kcldata,"Adel_EIS_kcl.csv")



