#####       SPEAK VISUALLY          #####
#####       WORKSHOP 2018           #####
##### JUDIT MOKOS mokjud@gmail.com  ####
#----------------------------------------

##### INSTALL GGPLOT ####
#install.packages('ggplot2') #install ggplot2 package
library('ggplot2') #read ggplot2 package
stwd(yourfolder) #set your folder
#----------------------------------------
##### CRIMES IN BOSTON ####
  # Q: does the crime rate in Boston depend on the weather? 
##read the data
  boston<-read.csv("weather_crimes_Boston.csv")
  boston$Date<- as.Date(boston$Date, "%m/%d/%y")  #tell to R column Date is date
  
  View(boston) #to see the datatable
  str(boston) #structure of the data
  summary(boston) #quick summary of the data
  head(boston) #first 5 row
  
##basic R plots  
  hist(boston$nb_crimes)
  plot(x=boston$Date, y=boston$nb_crimes)
  plot(x=boston$TEMP, y=boston$nb_crimes)
  plot(x=boston$Day_Wk, y=boston$nb_crimes)    
  plot(x=boston$year, y=boston$nb_crimes)
  plot(x=boston$Snow, y=boston$nb_crimes)  
  
##ggplot
  ##1. first layer
    ggplot(data=boston, aes(y=nb_crimes, x=Date))
    
  ##2. scatterplot
    ggplot(data=boston, aes(y=nb_crimes, x=Date))+
      geom_point() #make scatterplot | try: geom_line, geom_area(), geom_smooth(), geom_blank(), geom_jitter(), geom_rug()
    
  ##3. add labels and title
    ggplot(data=boston, aes(y=nb_crimes, x=Date))+
      geom_point()+
      labs(title="Crimes in Boston", #add title
           y="number of crimes per day", #name of y axis
           x="year") #name of x axis
     
  ##4. colors
    ggplot(data=boston, aes(y=nb_crimes, x=Date, color=Season))+ #colored by season. color and fill always in asthetic.
            #try: color=Day_WK, color=Wind. it is easy to change it. 
      geom_point()+
      labs(title="Crimes in Boston",y="number of crimes per day",x="year")
    
  ##5. customize colors and axis
    ggplot(data=boston, aes(y=nb_crimes, x=Date, color=Season))+
      geom_point()+
      labs(title="Crimes in Boston",y="number of crimes per day",x="year")+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "1 month")+ #customize axis
              #try: date_minor_breaks = "1 years"
              #try: date_breaks = "1 month", date_labels = "%m", date_minor_breaks = "1 month"
      scale_color_manual(values=c("#f5b700", "#04e762",  "#f42272","#00a1e4"), #customize colors. try: "red", "blue"
                         breaks=c("Spring", "Summer","Fall", "Winter" ), 
                         name="Colored by season")+      #change the order of the legend
          #customize the parameter of asthetics: scale_x_continous, scale_x_discrete, scale_x_date, scale_x_manual
          #scale_y_continous, scale_color_continous, scale_fill_manual
      theme_light() #change theme | try: theme_dark(), theme_blank(), theme_classic(), theme_grey, theme_minimal
      
      
  ##6. add text
    ggplot(data=boston, aes(y=nb_crimes, x=Date, color=Season))+
      geom_point()+
      labs(title="Crimes in Boston",y="number of crimes per day",x="year")+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "1 month")+
      theme_light()+ 
      scale_color_manual(values=c("#f5b700", "#04e762",  "#f42272","#00a1e4"), breaks=c("Spring", "Summer","Fall", "Winter" ))+
      annotate("text", x=(boston$Date[which(substring(boston$Date, 6)=="12-25")]), #add text
               y= boston$nb_crimes[which(substring(boston$Date, 6)=="12-25")] , # y coordinate of the text
               label=c(rep("christmas", 5)), #text
               size=3) #change size
    
# STOP #  
#---------------------------------------------------------------

##### BASKETBALL PLAYERS ####
  # Q: are really basketball players taller than the average? 
##read the data 
  heights<-read.csv("height_basket_vs_football.csv")
  View(heights)
  str(heights)
  head(heights)
  
##basic R plots
  hist(heights$football) 
  hist(heights$basketball)

##prepare the data for ggplot
  heights_cm<-30.48*heights  #change the feet to cm. 1feet=30.48 cm
  heights2<-data.frame(cm=c(heights_cm$football[!is.na(heights_cm$football)], heights_cm$basketball[!is.na(heights_cm$basketball)]), #make a column call 'cm' 
                      game=c(rep("football", length(heights_cm$football[!is.na(heights_cm$football)])), rep("basketball", length(heights_cm$basketball[!is.na(heights_cm$basketball)])))) #make a column call 'game'
  View(heights2)
  str(heights2)
  
  
##ggplot
  ##1. make the first layer
    ggplot(data=heights2, aes(x=cm)) #because we will make a histogram, to set y axis is not necesary

  ##2. set graph type
    ggplot(data=heights2, aes(x=cm))+
      geom_histogram()  #histogram | try: geom_density(), geom_dotplot(), geom_freqpoly(), geom_bar()
  
  ##3. color by group
    ggplot(data=heights2, aes(x=cm, fill=game))+ #color the bars by game. try this: aes(x=cm, color=game)
      geom_histogram(binwidth = 5) #set the width of the bars. try: 10
    

    
  ##4. change the position of the bars
    ggplot(data=heights2, aes(x=cm, fill=game))+ 
      geom_histogram(binwidth = 5, 
                     alpha=0.5, #transparency try: 1, 0.7
                     position="dodge") #try this: position="identity", "stack", "fill"
  ##5. add mean lines
    #calculate the means: 
    
    basketball.mean<-mean(heights2$cm[which(heights2$game=="basketball")]) #calculate the average height of a basketball player
    football.mean<-mean(heights2$cm[which(heights2$game=="football")]) #calculate the average height of a football player
    
    heights2.mean<-data.frame(mean=c(basketball.mean, football.mean), game=c("basketball", "football")) #make a dataframe
    
    #graph
    ggplot(data=heights2, aes(x=cm, fill=game))+ 
      geom_histogram(binwidth = 5, alpha=0.5, position="dodge")+
      geom_vline(data=heights2.mean, aes(xintercept=mean, color=game), #adding vertical line and color them
                 linetype="dashed") 
    
  ##7. add density plot
    ggplot(data=heights2, aes(x=cm,  fill=game, color=game))+ #make y density instead of count. dont forget the dots!
      geom_histogram(aes(y=..count..), #y axis should be count and not frequency. 
                     binwidth = 5, alpha=0.5, position="dodge")+
      geom_density(aes(y=..count..*5), #add a density plot, y axis should be count. you need to multiple the count by the binwidth
                   alpha=0.2) #make it transparent. 
    

  ##8.legend
    ggplot(data=heights2, aes(x=cm,  fill=game, color=game))+ #make y density instead of count. dont forget the dots!
      geom_histogram(aes(y=..count..), binwidth = 5, alpha=0.5, position="dodge")+
      geom_density(aes(y=..count..*5), alpha=0.2)+
      scale_fill_discrete(name="What is he playing?")+ #change the title of the legend
      scale_color_discrete(name="What is he playing?")+
      scale_y_continuous(breaks=seq(from=0, to=20, by=2))+
      scale_x_continuous(breaks=seq(from=140, to=220, by=5))
    
  ##9. 
    #png("J:/your folder / figures/basketball.png")
    ggplot(data=heights2, aes(x=cm,  fill=game, color=game))+ #make y density instead of count. dont forget the dots!
      geom_histogram(aes(y=..count..), binwidth = 5, alpha=0.5, position="dodge")+
      geom_density(aes(y=..count..*5), alpha=0.2)+
      theme_classic()+
      labs(title="Are basketball players tall?", x="heights2 (cm)", y="number of players")+
      geom_vline(data=heights2.mean, aes(xintercept=mean, color=game), linetype="dashed", size=1, show.legend=F) +
      scale_colour_manual(name="What is he playing?", values=c(basketball="#ef8a17", football="#09a129"), label=c("basketball player", "football player"))+
      scale_fill_manual(name="What is he playing?", values=c(basketball="#ef8a17", football="#09a129"), label=c("basketball player", "football player"))+
      scale_y_continuous(breaks=seq(from=0, t=20, by=5))+
      scale_x_continuous(breaks=seq(from=140, to=220, by=5))
    #dev.off()
    
    
# STOP #    
#---------------------------------------------------------------
# additional tips

##### BASKETBALL PLAYERS VS FOOTBALL PLAYERS #####
  ##making a boxplot
    ggplot(data=heights2, aes(x=game, y=cm, fill=game))+
      geom_boxplot()+ #boxplot
      theme_classic()+ #theme
      scale_fill_manual(values=c("#ef8a17","#87d68d" ))+ #customize color
      stat_summary(fun.y=mean, geom="point", shape=3, size=6, col="#034732")+ #add mean
      ggtitle("Who is taller?")+ #title
      labs(x="", y="height (cm)")+ #axis label
      guides(fill=F)+ #remove legend
      scale_x_discrete(labels = c('basketball players','football players'))+ #customize x axis
      theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) #change the size of the text

    
##### RESEARCH EXPENSES ####
  ## read data ##
    #research<-read.csv("ksh_research.csv")
    str(research)
  ## basic R ##
    plot(x=research$year, y=research$total, col="blue", pch=19) #try: pch=8

    
  ## making scatterplot
    ggplot(data=research, aes(x=year, y=total))+
      geom_point(color="#008bf8", size=4)+ #scatterplot, customize color and size
      ggtitle("Research and innovation expenses in Hungary")+ #add title
      labs(x="", y="million euro")+ #customize axis labels
      scale_y_continuous(labels=function(x) format(x, scientific = F))+  #change y axis to normal numbers
      theme_classic()+
      annotate("text", #add text
               x=2015, y=research$total[which(research$year==2015)]+10000, # place of the text
               label="2015")+ #the text itself
      annotate("text", x=2016, 
               y=research$total[which(research$year==2016)]-10000,
               label="2016")+
      geom_segment(aes(x=2015, y=research$total[which(research$year==2015)], #add arrow
                       xend=2016, yend=research$total[which(research$year==2016)]), #from x,y to xend,yend
                   arrow=arrow(length=unit(0.30, "cm"), type="closed"),  #set the type
                   color="#ff7f11", size=1) #customize color and size
    

##### WORLDRANKING #####
  ## is it true that people with better internet are happier?   
    
  ##data:   
    worldrankings<-read.csv("worldkrankings.csv")
    worldrankings 
    str(worldrankings)
  
  ##ggplot:     
    ggplot(data=worldrankings, aes(x=happiness, y=internet_speed))+
      geom_point()+
      geom_smooth(method="lm")+
      ggtitle("better internet - happier people?")+
      labs(x="average happiness", y="average internet speed")+
      theme_minimal()
  
    