library(shiny)
library(shinydashboard)
library(dplyr)
library(SnowballC)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(ggmap)
library(googleway)
library(ggalt)
library(factoextra)
library(cluster)
library(plotly)
library(highcharter)
library(r2d3)
library(config)
#map
library(sp)
library(rgeos)
library(maptools)
library(stringr)

library(randomForest)
register_google(key = "AIzaSyA4ivbGjkfXQuL9qtFXNNhFFMoIE7Macz4")

#generate 50 random numbers
# s=sample(1000:500,50)
# #mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/in/in-all.js"))
# #glimpse(mapdata)
# 
# # x=na.omit(data())

options(shiny.maxRequestSize = 30*1024^2)

server <- function(input, output,session) {
  

  #foo[foo==""] <- NA
  
  #if(!is.null(inFile))
  #{
  
  
  #wordcloud
  
  
  
  # }
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = T, sep = ',')
    output$wordcloud=renderPlot(
      {
        
        text <- readLines(inFile$datapath)
        docs <- Corpus(VectorSource(text))
        inspect(docs)
        toSpace<-content_transformer(function(x,pattern)gsub(pattern,"",x))
        docs <- tm_map(docs,toSpace,"/")
        docs <- tm_map(docs,toSpace,"@")
        docs <- tm_map(docs,content_transformer(tolower))
        docs <- tm_map(docs,removeNumbers)
        docs <- tm_map(docs,removeWords,stopwords("english"))
        docs <- tm_map(docs,removeWords,c("blabla1","blabla2"))
        docs <- tm_map(docs,removePunctuation)
        docs <- tm_map(docs,stripWhitespace)
        docs <- tm_map(docs,stemDocument)
        
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing = TRUE)
        d <- data.frame(word=names(v),freq=v)
        head(d,10)
        set.seed(1234)
        wordcloud(words = d$word,freq = d$freq,min.freq = 1,max.words = 900,random.order = FALSE,rot.per = 0.15,colors = brewer.pal(20,"Dark2"))
      }
    )
    
    
    
   updateSelectInput(session,inputId = 'animalid', label='Year of adoption', choices = df$year.of.adoption, selected = df$year.of.adoption)
    updateSelectInput(session,inputId = 'statusid', label='status', choices = df$status.of.animal, selected = df$status.of.animal)
    updateSelectInput(session,inputId = 'statuspieid', label='Status', choices = df$status.dead, selected = df$status.dead)
    updateSelectInput(session,inputId = 'genderid', label='gender', choices = df$gender, selected = df$gender)
    updateSelectInput(session ,inputId = 'col',label = "Intake Type",choices = df$Intake.Type,selected = df$Intake.Type)
    updateSelectInput(session, inputId = 'diseaseid',label='disease', choices = df$Diseases.total.year, selected = df$Diseases.total.year) 
    updateSelectInput(session,inputId = 'vaccyr', label='vaccination year', choices = df$year.of.vaccination, selected = df$year.of.vaccination)
    updateSelectInput(session, inputId = 'pieid',label='select Year', choices = df$Diseaseyear, selected = df$Diseaseyear) 
    updateSelectInput(session, inputId = 'piestatusyr',label='select Year', choices = df$Diseaseyear, selected = df$Diseaseyear) 
    updateSelectInput(session, inputId = 'diseaselinear',label='Diseases', choices = df$Diseases.total.year, selected = df$Diseases.total.year) 
    updateSelectInput(session, inputId = 'diseasedoglinear',label='Diseases', choices = df$Diseases.total.year, selected = df$Diseases.total.year) 
    updateSelectInput(session, inputId = 'yrdeath',label='year', choices = df$Diseases.year, selected = df$Diseases.year) 
    return(df)
    na.omit(df)
  })
  

  
  
  #table in data
   #  output$test <- renderTable(
   #  data(),striped =TRUE, hover = TRUE, bordered = TRUE,
   # )

  #adoption
  
  observe({
    
    
    output$plot1 = renderPlotly({
      #  a<-data() %>%filter(data()$year.of.adoption== input$animalid) %>%select(name,date.of.adoption,animal)
      # b<-data.frame(list(c(a)))
      # p <-  ggplot(b,aes(x=name,y=date.of.adoption,fill=animal)) + geom_bar(stat="identity", position=position_dodge(), colour="black")
      # p
      # #+geom_bar(stat = "identity")+scale_fill_brewer(palette="Paired") # geom_point()+geom_line(aes(x=name),color="red",group=1)
      #  p + theme(axis.text.x =
      #              element_text(size  = 10,
      #                            angle=90,
      #                           hjust = 1,
      #                           vjust = 1)) + geom_text(aes(label=date.of.adoption),vjust=-0.5, color="black", size=3,position = position_nudge(y = -0.1))#+theme_minimal()
      #  
      
      # p <-  ggplot(data(),mapping = aes(x=animal,y=year.of.adoption)) + geom_line()#+ facet_wrap(facets = vars(year.of.adoption))
      # p
      
      # 
      # g <- ggplot(data(), aes(total.year,total.puppy,total.dog))
      # g <- g + geom_point(aes(y=total.puppy), colour="red")+geom_line(aes(y=total.puppy),color='red',group=1)+ geom_text(aes(label=total.puppy),vjust=-1.5, color="red", size=5,position = position_nudge(y = -0.1))
      # g <- g + geom_point(aes(y=total.dog),color="green")  +geom_line(aes(y=total.dog),color='green')+geom_text(aes(label=total.dog),vjust=1.5, color="green", size=5,position = position_nudge(y= -0.1))
      # g+ theme(axis.text.x =
      #            element_text(size  = 10,
      #                         angle = 45,
      #                         hjust = 1,
      #                         vjust = 1),legend.position = "top")#+ ylab('Number of adoption')+xlab('Year')
      # 
      # g
      
      
      # data <- data.frame(x, trace_0, trace_1, trace_2)
      
      p <- plot_ly(data(), x = ~total.year, y = ~total.dog, name = 'Total Dog', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = ~total.puppy, name = 'Total Puppy', mode = 'lines+markers') %>% layout(xaxis=list(title='Year'),yaxis=list(title='count'))
      # add_trace(y = ~total.puppy, name = 'trace 2', mode = 'markers')
      
      #g <- plot_ly(data(),x=~total.year,y=~total.puppy,type = "scatter",mode="lines",linetype = ~total.dog)
      
    })
    
    #disease
    output$plot2 <-renderPlotly({
      a<-data() %>%filter(data()$Diseases.total.year== input$diseaseid) %>%select(Diseases.total.year,Diseaseyear,Disease.total.dog,Disease.total.puppy)
      b<-data.frame(list(c(a)))
      #print(b)
      dfm <- melt(b[,c('Diseaseyear','Disease.total.puppy','Disease.total.dog')],id.vars = 1)
     # print(dfm)
      g <- ggplot(dfm,aes(x= Diseaseyear,y= value)) + 
        geom_bar(aes(fill = variable),stat = "identity",position = "dodge") 
      #+scale_y_log10()# + scale_x_continuous(labels=c("2013", "2014", "2015", "2016", "2017", "2018"))# + scale_y_continuous(breaks=seq(0,50,100))
      g
      # g <- ggplot(b, aes(Diseaseyear,Disease.total.puppy,Disease.total.dog))
      # g <- g + geom_point(aes(y=Disease.total.puppy), colour="red")+geom_line(aes(y=Disease.total.puppy),color='red',group=1) #+ geom_text(aes(label=Disease.total.puppy),vjust=-1.5, color="red", size=5,position = position_nudge(y = -0.1))
      # g <- g + geom_point(aes(y=Disease.total.dog),color="green")  +geom_line(aes(y=Disease.total.dog),color='green',group=1) #+ geom_text(aes(label=Disease.total.dog),vjust=1.5, color="green", size=5,position = position_nudge(y= -0.1))
      #  g+ theme(axis.text.x =
      #              element_text(size  = 10,
      #                           angle = 45,
      #                           hjust = 1,
      #                           vjust = 1),legend.position = "top")#+ ylab('Number of adoption')+xlab('Year')
      # 
      # g
      
      
    })
    output$diseasevalue <- renderValueBox({
      a<-data() %>%filter(data()$Diseases.total.year== input$diseaseid) %>%select(Diseases.total.year,Diseaseyear,Disease.total.dog,Disease.total.puppy)
      b<-data.frame(list(c(a)))
      sum_dog <- b%>%group_by(input$diseaseid)%>%summarise(total=sum(Disease.total.dog))
      # print(sum_dog)
      sum_dog%>%tally(sum_dog$total)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Total Count of Dog ",color = "purple")
      
      
    })
    
    output$diseasevaluepuppy <- renderValueBox({
      a<-data() %>%filter(data()$Diseases.total.year== input$diseaseid) %>%select(Diseases.total.year,Diseaseyear,Disease.total.dog,Disease.total.puppy)
      b<-data.frame(list(c(a)))
      sum_puppy <- b%>%group_by(input$diseaseid)%>%summarise(total=sum(Disease.total.puppy))
      # print(sum_dog)
      sum_puppy%>%tally(sum_puppy$total)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Total Count of Puppy ",color = "purple")
      
    })
    
    
    #vaccination
    output$vaccplot = renderPlotly({
      a<-data() %>%filter(data()$year.of.vaccination==input$vaccyr) %>%select(Area,Vaccinated)
      b<-data.frame(list(c(a)))
      count<- b %>%group_by(Area)%>%summarise(total=sum(Vaccinated))
      #print(count)
      p <-  ggplot(count,aes(Area,total)) +geom_bar(stat = "identity") # geom_point()+geom_line(aes(x=name),color="red",group=1)
      p + theme(axis.text.x =
                  element_text(size  = 10,
                               angle=90,
                               hjust = 1,
                               vjust = 1)) #+ geom_text(aes(label=total),vjust=-0.5, color="black", size=3,position = position_nudge(y = -0.1))
      
    })
    
    output$vaccvalue <- renderValueBox({
      a<-data() %>%filter(data()$year.of.vaccination==input$vaccyr) %>%select(Area,Vaccinated)
      b<-data.frame(list(c(a)))
      sum_vacc <- b%>%group_by(input$vaccyr)%>%summarise(total=sum(Vaccinated))
      # print(sum_dog)
      sum_vacc%>%tally(sum_vacc$total)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Total Vaccination ",color = "purple")
      
    })
    
    #status
    output$statusplot=renderPlotly({
      a<-data() %>%filter(data()$status.of.animal== input$statusid) %>%select(status.year,status.of.animal,status.of.dog,status.of.puppy)
      b<-data.frame(list(c(a)))
      #print(b)
      dfm <- melt(b[,c('status.year','status.of.dog','status.of.puppy')],id.vars = 1)
      #print(dfm)
      g <- ggplot(dfm,aes(x= status.year,y= value)) + 
        geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + xlab("Year")
      #+ scale_y_log10()# + scale_x_continuous(labels=c("2013", "2014", "2015", "2016", "2017", "2018"))# + scale_y_continuous(breaks=seq(0,50,100))
      g
    })
    
    output$statusvalue <- renderValueBox({
      a<-data() %>%filter(data()$status.of.animal== input$statusid) %>%select(status.year,status.of.animal,status.of.dog,status.of.puppy)
      b<-data.frame(list(c(a)))
      sum_dog <- b%>%group_by(input$statusid)%>%summarise(total=sum(status.of.dog))
     # print(sum_dog)
      sum_dog%>%tally(sum_dog$total)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Total Count of Dog ",color = "purple")
    })
    
    output$statusvaluepuppy <- renderValueBox({
      a<-data() %>%filter(data()$status.of.animal== input$statusid) %>%select(status.year,status.of.animal,status.of.dog,status.of.puppy)
      b<-data.frame(list(c(a)))
      sum_puppy <- b%>%group_by(input$statusid)%>%summarise(total=sum(status.of.puppy))
      # print(sum_dog)
      sum_puppy%>%tally(sum_puppy$total)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Total Count of puppy ",color = "purple")
      
      
    })
    
    
    
    #gender
    output$view_male=renderPlotly({
      a<-data() %>%filter(data()$gender== "M") %>%select(gender.year,gender,gender.dog,gender.puppy)
      b<-data.frame(list(c(a)))
      #print(b)
      dfm <- melt(b[,c('gender.year','gender.dog','gender.puppy')],id.vars = 1)
      #print(dfm)
      g <- ggplot(dfm,aes(x= gender.year,y= value)) + 
        geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +xlab("Year") #+ xlim(2013,2020)
      #scale_y_log10()# + scale_x_continuous(labels=c("2013", "2014", "2015", "2016", "2017", "2018"))# + scale_y_continuous(breaks=seq(0,50,100))
      g
    })
    output$view_female=renderPlotly({
      a<-data() %>%filter(data()$gender== "F") %>%select(gender.year,gender,gender.dog,gender.puppy)
      b<-data.frame(list(c(a)))
      #print(b)
      dfm <- melt(b[,c('gender.year','gender.dog','gender.puppy')],id.vars = 1)
      #print(dfm)
      g <- ggplot(dfm,aes(x= gender.year,y= value)) + 
        geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+xlab("Year") #+ 
      # scale_y_log10()# + scale_x_continuous(labels=c("2013", "2014", "2015", "2016", "2017", "2018"))# + scale_y_continuous(breaks=seq(0,50,100))
      g
    })
    
    #donation
    output$Min_ <- renderValueBox({
      data()%>%tally(data()$AMOUNT)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Total Donation (In Rupees)",color = "purple",icon = icon("angle-double-right"))
      # valueBox(value = min(s),
      #          subtitle =" Total Donation")
    })
    output$top_donation <- renderD3({
      data()%>%group_by(STATE,AMOUNT)%>%
        tally()%>%
        collect()%>%
        arrange(desc(n))%>%
        head(10)%>%
        arrange(AMOUNT)%>%
        mutate(AMOUNT=str_sub(AMOUNT,1,30))%>%
        rename(
          x=STATE,
          y=n,
          label=AMOUNT
        )
      r2d3("bar_plot.js")
    })
    
    output$donationtable <- renderTable({
      # a <- data()
      # print(head(a))
      # b<-a[42:43]
      # print(head(b))
      count<- data() %>%group_by(STATE)%>%summarise(total=sum(AMOUNT))
      c <- count%>% arrange(desc(total))
     # print(head(c,10))
    } )
    # 
    # output$Max_ <- renderValueBox({
    #   valueBox(value = max(s),
    #            subtitle = "Maximum",
    #            color = "purple",
    #            icon = icon("credit-card"))
    # })
    # 
    # output$mean_ <- renderInfoBox({
    #   infoBox(value = mean(s),
    #           title = "mean",
    #            subtitle = "Maximum",
    #            color = "purple",
    #            icon = icon("angle-double-right"))
    # })
    # 
    
    
    
    #linear
    output$notpuppy <- renderPlotly({
      ggplot(data(),aes(stat.yr,not.puppy))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    })
    output$summarynotpuppy <- renderPrint({
      linear<-lm(stat.yr~not.puppy,data())
      summary(linear)
    })
    output$predictnotpuppy <- renderPrint({
      linear<-lm(stat.yr~not.puppy,data())
      # reduce <- lm(status.of.dog~status.of.puppy,data())
      # full <- lm(status.of.dog~status.of.puppy,data())
      # anova(reduce,full)
      predict(linear,data.frame(not.puppy=17),interval='confidence')
    })

#####################################################################################################
    #linear
    # #admit
    # output$admitpuppy<- renderPlot({
    #   ggplot(data(),aes(stat.yr,admitted.puppy))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summaryadmitpuppy <- renderPrint({
    #   linear1<-lm(stat.yr~admitted.puppy,data())
    #   summary(linear1)
    # })
    # #released
    # output$releasedpuppy <- renderPlotly({
    #   ggplot(data(),aes(stat.yr,released.puppy))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summaryreleasedpuppy <- renderPrint({
    #   linear2<-lm(stat.yr~released.puppy,data())
    #   summary(linear2)
    # })
    # 
    # #dead
    # output$deadpuppy <- renderPlotly({
    #   ggplot(data(),aes(stat.yr,dead.puppy))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summarydeadpuppy <- renderPrint({
    #   linear3<-lm(stat.yr~dead.puppy,data())
    #   summary(linear3)
    # })
    # 
    # #dog
    # #not found
    # output$notdog <- renderPlotly({
    #   ggplot(data(),aes(stat.yr,not.dog))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summarynotdog <- renderPrint({
    #   linear<-lm(stat.yr~not.dog,data())
    #   summary(linear)
    # })
    # output$predictnotdog <- renderPrint({
    #   linear<-lm(stat.yr~not.dog,data())
    #   predict(linear,data.frame(not.dog=88),interval='confidence')
    # })
    # 
    # #admit
    # output$admitdog<- renderPlot({
    #   ggplot(data(),aes(stat.yr,admitted.dog))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summaryadmitdog <- renderPrint({
    #   linear1<-lm(stat.yr~admitted.dog,data())
    #   summary(linear1)
    # })
    # #released
    # output$releaseddog <- renderPlotly({
    #   ggplot(data(),aes(stat.yr,released.dog))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summaryreleaseddog <- renderPrint({
    #   linear2<-lm(stat.yr~released.dog,data())
    #   summary(linear2)
    # })
    # 
    # #dead
    # output$deaddog <- renderPlotly({
    #   ggplot(data(),aes(stat.yr,dead.dog))+geom_point()+geom_smooth(method = lm)+xlab("Year")+ylab("count")
    # })
    # output$summarydeaddog <- renderPrint({
    #   linear3<-lm(stat.yr~dead.dog,data())
    #   summary(linear3)
    # })

    #statuslinear
    output$statusid <- renderPlot({
      linear2<-lm(status.of.puppy~status.of.dog,data())
     # print(summary(linear2))
      ggplot(data(),aes(status.of.puppy,status.of.dog))+geom_point()+geom_smooth(method = lm)+xlab("status of puppy")+ylab('status of dog')
    })
    output$summarystatus <- renderPrint({
      model <- lm(status.of.puppy~status.of.dog,data())
      summary(model)
    })


    #linear
    # output$dogdiseases<- renderPlot({
    #   disease_sum<- data()%>%group_by(Diseases.total.year)%>%summarise(total=sum(Disease.total.dog))
    #   print(disease_sum)
    #   disease_sum$Diseases.total.year=as.numeric(disease_sum$Diseases.total.year)
    #   str(disease_sum)
    #   #linear3<-lm(Diseases.total.year~Disease.total.dog,data())
    #   #print(summary(linear3))
    #   ggplot(disease_sum,aes(Diseases.total.year,total))+geom_point()+geom_smooth(method = lm)+xlab("Year")
    # })
    # output$summarydiseasedog <- renderPrint({
    #   disease_sum<- data()%>%group_by(Diseases.total.year)%>%summarise(total=sum(Disease.total.dog))
    #   print(disease_sum)
    #   disease_sum$Diseases.total.year=as.numeric(disease_sum$Diseases.total.year)
    #   str(disease_sum)
    #   linear3<-lm(Diseases.total.year~total,disease_sum)
    #   # print(summary(linear3))
    #   summary(linear3)
    # })
    # output$predictdiseasedog <- renderPrint({
    #   linear<-lm(Diseases.total.year~total,disease_sum,data())
    #   # reduce <- lm(status.of.dog~status.of.puppy,data())
    #   # full <- lm(status.of.dog~status.of.puppy,data())
    #   # anova(reduce,full)
    #   predict(linear,data.frame(not.puppy=17),interval='confidence')
    # })
#############################################################################################
    
    #lineardisease
    output$diseaselinearplot <- renderPlotly({
      
      a<-data() %>%filter(data()$Diseases.total.year== input$diseaselinear) %>%select(Diseases.total.year,Disease.total.puppy,Diseaseyear)
      b<-data.frame(list(c(a)))
      vacc_sum <- b%>%group_by(Diseaseyear)%>%summarise(total=sum(Disease.total.puppy))
     # print(vacc_sum)
      ggplot(vacc_sum,aes(Diseaseyear,total))+geom_point()+geom_smooth(method = lm)+xlab("Year")
      
    })
    # output$summaryvacc <- renderPrint({
    #  a <-data() %>%filter(data()$Diseases.total.year== input$diseaselinear) %>%select(Disease.total.puppy,Diseaseyear)
    # b<-data.frame(list(c(a)))
    # vacc_sum <- b%>%group_by(Diseaseyear)%>%summarise(total=sum(Disease.total.puppy))
    # print(vacc_sum)
    #     model <- lm(Diseaseyear~Disease.total.puppy,vacc_sum)
    #     summary(model)
    #   })
    # 
    
    output$diseasedoglinearplot <- renderPlot({
      
      a<-data() %>%filter(data()$Diseases.total.year== input$diseasedoglinear) %>%select(Diseases.total.year,Disease.total.dog,Diseaseyear)
      b<-data.frame(list(c(a)))
    #  print(b)
      vacc_sum <- b%>%group_by(Diseaseyear)%>%summarise(total=sum(Disease.total.dog))
      #print(vacc_sum)
      ggplot(vacc_sum,aes(Diseaseyear,total))+geom_point()+geom_smooth(method = lm)+xlab("Year")
      
    })
    # output$summarydog <- renderPrint({
    #   a <-data() %>%filter(data()$Diseases.total.year== input$diseasedoglinear) %>%select(Disease.total.dog,Diseaseyear)
    #   b<-data.frame(list(c(a)))
    #   vacc_sum <- b%>%group_by(Diseaseyear)%>%summarise(total=sum(Disease.total.dog))
    #   print(vacc_sum)
    #   model <- lm(Diseaseyear~Disease.total.dog,vacc_sum)
    #   summary(model)
    # })
#########################################################################    
    #piechart
    output$pie <- renderPlotly({
      a<-data() %>%filter(data()$Diseaseyear== input$pieid) %>%select(Diseaseyear,Diseases.total.year,Disease.total.puppy)
      b<-data.frame(list(c(a)))
      p <- plot_ly(b, labels = ~Diseases.total.year, values = ~Disease.total.puppy, type = 'pie') %>%
        layout(title = 'Diseases of Puppy',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    output$piedog <- renderPlotly({
      a<-data() %>%filter(data()$Diseaseyear== input$pieid) %>%select(Diseaseyear,Diseases.total.year,Disease.total.dog)
      b<-data.frame(list(c(a)))
      p <- plot_ly(b, labels = ~Diseases.total.year, values = ~Disease.total.dog, type = 'pie') %>%
        layout(title = 'Diseases of Dog',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    output$statuspiedog <- renderPlotly({
      a<-data() %>%filter(data()$Diseaseyear== input$piestatusyr & data()$status.dead== "Dead") %>%select(Diseaseyear,Diseases.total.year,Disease.total.dog,status.dead)
      b<-data.frame(list(c(a)))
     # print(b)
      p <- plot_ly(b, labels = ~Diseases.total.year, values = ~Disease.total.dog, type = 'pie') %>%
        layout(title = 'Status of Dog',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    output$Piefund <- renderPlotly({
      p <- plot_ly(data(), labels = ~STATE, values = ~AMOUNT, type = 'pie') %>%
        layout(
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
    })
    
  })
  
  observe({
    
    #cluster
    a <- data()
    b<-a[12:15]
    b1<-a[12:13]
    d<-na.omit(b)
    d1<-na.omit(b1)
    #print(d)
    sum(is.na(d))
    c1<-d[-1:-2]
   # print(head(c1))
    e<-na.omit(data.frame(list(c(c1))))
    #print(e)
    sum(is.na(e))
    set.seed(123)
    final <- kmeans(e, 4, nstart = 25)
   # print(final)
    
    #agreegate
    a<- data()%>%filter(data()$status.of.animal== "Dead")%>%select(status.of.animal,status.of.dog,status.of.puppy,status.year)
   # print(a)
    count<- a%>%group_by(status.of.animal,status.year)%>%summarise(total=sum(status.of.dog,status.of.puppy))
    #print(count)
    
    output$cluster <- renderPlot({
       
       fviz_cluster(final, data = e)
      #print(cbind(d,final$cluster))

      #stringsAsFactors = FALSE
    })
    
    output$clustertabel <- renderDataTable(
      
      cbind(d1,final$cluster)
    )
    
    output$agreegate <- renderPlotly({
      count%>% 
        #Step 2
        group_by(status.of.animal,status.year)%>% 
        #Step 3
        summarise(Average = mean(total))%>% 
        #Step 4
        ggplot(aes(x = status.year, y = Average, fill = status.year)) +
        geom_bar(stat = "identity") +
        theme_classic() +
        labs(
          x = "Year",
          y = "Count",
          title = paste(
            "Agreegate group_by(Year) with summarise(Status death)"
          )
        )
    })
    
    
    
    output$avgyr_ <- renderValueBox({
     #glimpse(count)
      a<- summarise(count, Average =mean(total))
      #summary(summary1)
      a%>%tally(a$Average)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Average Year",color = "purple")
      
      #b <- data.frame(a/12)
      #print(b)
      
      # ex1 <- count%>%
      #   group_by(status.year)%>%
      #   summarise(Average = mean(total))
      # head(ex1)
      # print(ex1)
      # ex1
    })
    
    output$avgmonth <-renderValueBox({
      a<- summarise(count, Average =mean(total))
      b <- data.frame(a/12)
      b%>%tally(b$Average)%>%pull()%>%as.integer()%>%prettyNum(big.mark = ",")%>%valueBox(subtitle =" Average Month",color = "purple")
    })
    
    output$statusagreegate <- renderPlotly({
      a<-data()%>%filter(data()$Diseases.year == input$yrdeath)%>%select(Status,Diseases.year)
      #print(a)
      b<-data.frame(list(c(a)))
      #print(b)
      
      
      # levels(b$Status)[levels(b$Status)=="Not Found"] <- "1"
      # levels(b$Status)[levels(b$Status)=="Released"] <- "2"
      # levels(b$Status)[levels(b$Status)=="Admitted"] <- "3"
      # levels(b$Status)[levels(b$Status)=="Dead"] <- "4"
      # 
      # print(levels(b$Status))
      # 
      # f <- bfactor(c("Not Found","Dead"))
      # levels(f) <- list("1" = "Not Found", "2" = "Dead")
      # print(levels(f))
      # print(b)
      # df <- b %>%
      #   mutate(Status = recode(Status, 
      #                           Not.Found = "1",
      #                           Admitted = "2",
      #                           Dead = "3")
      #   )
      # 
      # print(b)
      
      # ab<- b %>% group_by(input$yrdeath)%>% summarise(tt=sum(Status))
      # print(ab)
      # print(b)
      # status_factor<- factor(ab$Status)
      # factor(Status,ordered = TRUE,
      #        levels= c("Not Found","Released","Admitted","Dead"),
      #        levels=c("1","2","3","4")
      #        
      # )
      # print(head(status_factor))
       p <- plot_ly(b, labels = ~Status, values = ~count, type = 'pie') %>%
         layout(title = 'Status of Dog',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
    })
    
    
    output$map <- renderPlot({
      mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/in/in-all.js"))
      
      glimpse(mapdata)
      pop = as.data.frame(c(43200,0,14400,0,84000,10000,0,0,
                            0,0,3002160,0,5582000,0,0,72000,
                            14400,0,659200,0,0,1788800,0,0,
                            0,158400,0,8223200,0,120400,20000,73200,
                            2373680,26400))
      
      state= mapdata%>%
        select(`hc-a2`)%>%
        arrange(`hc-a2`)
      
      State_pop = as.data.frame(c(state, pop))
      names(State_pop)= c("State", "Population")
      
      hcmap("https://code.highcharts.com/mapdata/countries/in/in-all.js", data = State_pop, value = "Population",
            joinBy = c("hc-a2", "State"), name = "Donation",
            dataLabels = list(enabled = TRUE, format = '{point.name}'),
            borderColor = "#FAFAFA", borderWidth = 0.1,
            tooltip = list(valueDecimals = 0))
     (hcmap) 
      
      
      
      
    })
    
    # output$plot2= renderPlot({
    #   # chennai_ggl_road_map <- qmap("chennai", zoom=12, source = "google", maptype="roadmap")
    #   # chennai_ggl_road_map + geom_point(aes(x=name, y=date.of.adoption),
    #   #                                   data = places_loc, 
    #   #                                   alpha = 0.7, 
    #   #                                   size = 7, 
    #   #                                   color = "tomato") + 
    #   #   geom_encircle(aes(x=lon, y=lat),
    #   #                 data = places_loc, size = 2, color = "blue")
    #  # ggmap(get_map("Hannover, Germany"))
    #   set_key("GOOGLE_MAP_KEY")
    #   
    #   lat <- c(4,41) #India lat boundaries
    #   lon <- c(68,99) #India long boundaries
    #   center = c(mean(lat), mean(lon))
    #   
    #   google_map(location = center, zoom = 6)
    # })
    
  })
  
  

}