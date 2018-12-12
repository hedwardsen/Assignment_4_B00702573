library(rvest)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)
library(ggpubr)

#1) 
marathon = read_html("http://www.areep.com/results/live/overall.php?eid=37&rid=79")

#2)
marathon1 = marathon %>% html_nodes('td') %>% html_text() 

#3)
marathondf <- data.frame(matrix(unlist(marathon1), ncol=16, nrow=500, byrow=16))  

#4)
marathonnew = marathondf[,c(1,11,5,4,3,15,16,2,8,6,7,9,10,12,13,14)]

#5)
colnames(marathonnew) = c("Place","Sex_Place","Sex","Age","Name","Net_Time","Net_Pace","Bib","DIV","City",
                          "State","DIVPLACE","DIVTOTAL","SEXTOTAL","NETTIME","NETPLACE") 

#6)
marathonnew[7:16] <- list(NULL) #null stops the dataframe from listing the 7th through 16th column



#7)
marathonnew$Place = as.character(marathonnew$Place) #Place: factor to character
marathonnew$Place = as.numeric(marathonnew$Place) #Place: character to numeric
marathonnew$Sex_Place = as.character(marathonnew$Sex_Place) #Sex_Place: factor to character
marathonnew$Sex_Place = as.numeric(marathonnew$Sex_Place) #Sex_Place: character to numeric
marathonnew$Age = as.character(marathonnew$Age) #Age: factor to character
marathonnew$Age = as.numeric(marathonnew$Age) #Age: character to numeric

#8)
marathonnew$Sex = as.character(marathonnew$Sex) #Sex: factor to character
marathonnew$Net_Time = as.character(marathonnew$Net_Time) #Net_Time: factor to character


#18)
top50 = marathonnew$Place[1:50] #Top 50 Places
top50Sex = marathonnew$Sex[1:50] #Top 50 Sexes
top50Age = marathonnew$Age[1:50] #Top 50 Ages
top50df = data.frame(top50,top50Sex,top50Age) #Data frame of the top 50 runners


FigA = ggplot(data=top50df)+aes(top50,top50Age,fill=top50Sex)+geom_col(width=.85)+ 
  labs(title = "a", y="", x="")+theme(plot.margin = unit(c(0,0,0,0), "lines"))

FigA2 = FigA + theme(title = element_text(colour="black", size=8, face="bold",family="Helvetica"),
                     legend.position="none",
                     legend.title=element_blank(),
                     legend.text = element_text(size=6,family="Helvetica"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line.x = element_blank(), 
                     axis.text=element_text(colour="black",size=6,family="Helvetica"),
                     axis.ticks=element_blank()) + 
  scale_fill_manual(values=c('#A63452','#0A315C'), labels=c("Female","Male"))



last50 = marathonnew$Place[450:500] 
last50Sex = marathonnew$Sex[450:500] 
last50Age = marathonnew$Age[450:500] 
last50df = data.frame(last50,last50Sex,last50Age) 

FigC = ggplot(data=last50df)+aes(last50,last50Age,fill=last50Sex)+geom_col(width = .85)+ 
  labs(title = "c",  
       y="", x="Place")+theme(plot.margin = unit(c(0,0,0,0), "lines"))

FigC2 = FigC + theme(title = element_text(colour="black", size=8, face="bold",family="Helvetica"),
                     legend.position="none",
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line.x = element_blank(),
                     axis.text=element_text(colour="black", size=6, face='plain',family="Helvetica"),
                     axis.title.x=element_text(colour="black", size=7, face='plain', family="Helvetica"),
                     axis.ticks=element_blank()) + 
  scale_fill_manual(values=c('#A63452','#0A315C'))


FigB = ggplot(top50df, aes(x=top50Age,fill=top50Sex)) + geom_bar(width=.85)+
  labs(title = "b", x="", y="")+theme(plot.margin = unit(c(0,0,0,0), "lines"))

FigB2 = FigB + theme(title = element_text(colour="black", size=8, face="bold",family="Helvetica"),
                     legend.position=c(.8,.9),
                     legend.title=element_blank(),
                     legend.text=element_text(colour="black",size=7,family="Helvetica"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line.x = element_blank(),
                     axis.text.x = element_text(colour = "black",size=6,family="Helvetica"), 
                     axis.text.y = element_text(),
                     axis.text.y.right=element_blank(),
                     axis.ticks=element_blank())+ 
  scale_fill_manual(values=c('#A63452','#0A315C'),labels=c("Female","Male"))+
  scale_y_continuous(breaks = c(), sec.axis = sec_axis(~ . * 1, breaks = c(1,2,3,4,5)))+
  geom_hline(yintercept=1:5,color = "white",size=.75)

FigD = ggplot(last50df, aes(x=last50Age,fill=last50Sex)) + geom_bar(width=.85)+
  labs(title = "d", x="Age (yrs)", y="")+theme(plot.margin = unit(c(0,0,0,0), "lines"))

FigD2 = FigD + theme(title = element_text(colour="black", size=8, face="bold",family="Helvetica"),
                     axis.title.x = element_text(colour="black", size=7, face="plain",family="Helvetica"),
                     legend.position="none",
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line.x = element_blank(), 
                     axis.text.y = element_text(),
                     axis.text.y.right = element_blank(),
                     axis.text.x = element_text(colour="black",size=6,family="Helvetica"),
                     axis.ticks=element_blank())+ 
  scale_fill_manual(values=c('#A63452','#0A315C'))+
  scale_y_continuous(breaks = c(), 
                     sec.axis = sec_axis(~ . * 1, breaks = c(1,2,3,4,5)))+
  geom_hline(yintercept=1:4, color = "white",size=.75)








arrangeABCD = ggarrange(FigA2, FigB2, FigC2, FigD2, 
                        ncol=2, nrow=2, 
                        common.legend = FALSE)

arrangeABCD2 = annotate_figure(arrangeABCD, left = text_grob("Age (yrs)", 
                                                             color = "black", 
                                                             rot = 90, size=7),
                               right = text_grob("Frequency (# of runners)", color = "black", 
                                                 rot = -90, size=7),
                               bottom=text_grob(
                                 "Figure 1. a: the distribution of age amongst the top 50 placed runners; b: the proportion of gender and age within each age grouping of the first 50 placed runners; 
                                 white dashes divide each frequency value from 1 to 5. c: the distribution of age amongst the last 50 placed runners; d: the proportion of gender within each age 
                                 grouping of the last placed 50 runners; white dashes divide each frequency value from 1 to 4. Data from the Mount Desert Island half-marathon, Josh Merlis (2018).", size=7,just="center",family="Helvetica"))


arrangeABCD2










