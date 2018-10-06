source("global.R")
source("initialization.R")
#source("predictiton_svm.R")
source("finalfinal.R")
source("analytical.R")
#font_add_google("Hind", "dev")

dashboardInfo <-
  box(h2("Sarangi"),
      h4("//Headline//"),
      br(),
      h4(
        "Sarangi is a ",a(href = 'http://shiny.rstudio.com', 'Shiny'),
        "web application built on top of R for sentimental analysis of Nepali music"),
      h4("To get started, select something in the sidepanel."),
      br(),
      h4(HTML('&copy'),'2018 By ',a("Team Sarangi", href="#shiny-tab-about", "data-toggle" = "tab"))
  )

dashboardStats <-   fluidRow(
  infoBoxOutput("noOfSongs"),
  infoBoxOutput("noOfArtists")
)

tab_about <- fluidPage(
  tags$h1("Team Profile"),
  tags$h3("Supervisor"),            
  shiny::img(src = "dhiraj-sir.jpg",height = 200,width = 200),
  tags$h3("Project Members"),   
           tags$h4("Manasi Kattel"),
           shiny::img(src = "manasi.jpg",height = 150,width = 150),
           tags$h4("Araju Nepal"),
           shiny::img(src = "araju.jpg",height = 150,width = 150),
           tags$h4("Ayush Kumar Shah"),
           shiny::img(src = "ayush.jpg",height = 150,width = 150),
           tags$h4("Deepesh Shrestha"),
           shiny::img(src = "deepesh.jpg",height = 150,width = 150)
  
)

tab_upload <- fluidPage(title="Upload",
                        textInput("text_input", "Paste the song's lyrics"),
                        actionButton("upload_text", "Add"),
                        shiny::br(), shiny::br(), shiny::br(),
                        h1="Upload a document csv /text file",
                        fileInput("upload_file","Upload the file",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
                        radioButtons("upload_datset","Dataset Type",choices = c(csv="csv",txt="txt")),
                        h5("Max file is supported upto 5MB"),
                        radioButtons("upload_sep","Seperator",
                                     choices = c(comma=",",
                                                 tilde="~",
                                                 period=".",
                                                 minus="-")
                        ),
                        checkboxInput("check_upload","Header?"),
                        textOutput("show_upload")
)

tab_words_freq <- fluidPage(
  plotOutput("words_frequency")
)

tab_songs_emo <- fluidPage(
  selectInput("selectSong","Select any of the below songs",c("Parelima","Rato ra Chandra Surya","नसम्झ","नौंलाखे तारा","पानी पर्यो असीना झर्यो","वीर गोर्खाली")),
  plotOutput("songs_emo1")
)

# tab_overall_emo <- fluidPage(
#   plotOutput("overall_emo")
# )

tab_predicted <- fluidPage(
  fluidRow(
    box(  title = "Accuracy of prediction", width=12,tabBox( width=12,
      tabPanel("New Radial",textOutput("out1")),
      tabPanel("Linear",textOutput("out3")),
      tabPanel("Sigmoid",textOutput("out5"))
    ))
  ),
  
  fluidRow(
    box(  title = "Predicted Dataset",        width=12,tabBox(
      width=12,
      tabPanel("New Radial" , DT::dataTableOutput("testtable1")),
      tabPanel("Linear" , DT::dataTableOutput("testtable2")),
      tabPanel("Sigmoid",  DT::dataTableOutput("testtable3"))
    )),
    box( title = "Summary of prediction",   width=12,tabBox(
      width=12,
    
      tabPanel("New Radial",verbatimTextOutput("sum1")),
      tabPanel("Linear",verbatimTextOutput("sum2")),
      tabPanel("Sigmoid",verbatimTextOutput("sum3"))
    ))

  )

  
  # tags$h2("Accuracy"),
  # tags$p("New Radial"),textOutput("out1"),br(),
  # # tags$p("default radial"),textOutput("out2"),br(),
  # tags$p("Linear"),textOutput("out3"),br(),
  # # tags$p("poly"),textOutput("out4"),br(),
  # tags$p("Sigmoid"),textOutput("out5"),br(),
  # tags$h3("Test Data New Radial"),br(),
  # br(),
  # DT::dataTableOutput("testtable1"),
  # tags$h3("Test Data Linear"),
  # br(),
  # DT::dataTableOutput("testtable2"),
  # tags$h3("Test Data Sigmoid"),
  # br(),
  # DT::dataTableOutput("testtable3")
  # tags$h3("Test Data 4"),
  # br(),
  # DT::dataTableOutput("testtable4"),
  # tags$h3("Test Data 5"),
  # br(),
  # DT::dataTableOutput("testtable5"),
  # tags$h3("Test Data 6"),
  # br(),
  # DT::dataTableOutput("testtable6"),
  # tags$h3("Test Data 7"),
  # br(),
  # DT::dataTableOutput("testtable7")
)

tab_analytics <- fluidPage(
  tags$h3("Accuracy Graph"),
  plotOutput("analytical_accuracy")
)

tab_recommed <- fluidPage(
  selectInput("selectEmo","What is your mood ?",c("देश्भक्ती","हर्ष","दुःखी","प्रेम")),
  DT::dataTableOutput("show_rec")
)


#///////////////////////////SIDEBAR MENU TABS CONTENTS//////////////////////////////////////////////
menu_tabs <- tabItems(
  tabItem(tabName = "dashboard",fluidPage(dashboardInfo,dashboardStats)),
  tabItem(tabName = "wordcloud",vis_wordcloud <- fluidRow(box(width=12,wordcloud2Output("plot_wc")))),
  tabItem(tabName = "recommend",tab_recommed),
  tabItem(tabName = "prediction",tab_predicted),
  tabItem(tabName = "result",tableOutput("result")),
  tabItem(tabName = "analytics",tab_analytics),
  tabItem(tabName = "upload",tab_upload),
  tabItem(tabName = "dataset",DT::dataTableOutput("dataset")),
  tabItem(tabName = "frequency",tab_words_freq),
  # tabItem(tabName = "overall_emo",tab_overall_emo),
  tabItem(tabName = "songemo",tab_songs_emo),
  tabItem(tabName = "about",tab_about)
)

#///////////////////////////SIDEBAR MENU TABS//////////////////////////////////////////////
menus <- sidebarMenu(
  menuItem("Dashboard",tabName = "dashboard",icon = icon("home",lib="font-awesome")),
  menuItem("Upload",tabName = "upload",icon=icon("upload",lib="font-awesome")),
  menuItem("Recommend",tabName = "recommend",icon=icon("upload",lib="font-awesome")),
  menuItem("Prediction",tabName = "prediction",icon=icon("upload",lib="font-awesome")),
  menuItem("Prediction Result",tabName = "result",icon=icon("upload",lib="font-awesome")),
  menuItem("Analytics",tabName = "analytics",icon=icon("upload",lib="font-awesome")),
  menuItem("Dataset",tabName = "dataset",icon = icon("database")),
  menuItem("Most Frequent",tabName = "frequency",icon = icon("cloud")),
  menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
  menuItem("Song's Emotion",tabName = "songemo",icon = icon("bar-chart-o")),
  # menuItem("Overall Emotional",tabName = "overall_emo",icon = icon("bar-chart-o")),
  menuItem("About",tabName = "about",icon=icon("info",lib="font-awesome"))
)

header <- dashboardHeader(title="Sarangi",dropdownMenuOutput("msgOutput"))
body<-dashboardBody(menu_tabs)
sidebar <- dashboardSidebar(menus)
ui <- dashboardPage(title = "Sarangi", header, sidebar, body)

#///////////////////////SERVER CODE////////////////////////////////

server <- function(input,output,session){
  #InfoBOX
  
  listofartist <- reactive({
    lyrics_tidy%>%
      group_by(artist)
  })
  
  listofsong <- reactive({
    lyrics_dataset %>%
      filter(title %in% input$selectArtist) %>%
      group_by(title)
    
  })
  output$noOfSongs <- renderInfoBox(
    infoBox("No of Songs",nrow(dataset))
  )
  
  output$noOfArtists <- renderInfoBox(
    infoBox("No of Songs",nrow(dataset[1]))
  )
  
  output$dataset = renderDT(
    dataset, options = list(lengthChange = FALSE)
  )
  
  output$words_frequency <- renderPlot({
    z=lyrics_dataset %>%
      unnest_tokens(word, lyrics) %>%
      distinct() %>%
      count(word, sort = TRUE) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n))
    showtext.begin()
    #View(z)
    g = ggplot(data = z, aes(x = word,y=n)) +
      geom_col()+
      ggtitle("यो") +
      theme_bw(base_size = 30, base_family = "dev")
    print(g)
    showtext.end()
  })
  

  output$plot_wc <- renderWordcloud2({
    withProgress({
      setProgress(message="creating wordcloud...")
      wordcloud2(lyrics_words_counts[1:300, ], size = 1)   
    })
  })
 
 observeEvent(input$upload_text, {
   text <- input$text_input
   text$title <- "empty hunu hudaina"
   text$artist <- "empty hunu hudaina"
   text$sentiment <- "empty hunu hudaina"
   # View(text)
   names(text) <- c("lyrics", "title","artist","sentiment")
   
   write.csv(text,file="datasets/abc.csv")
   
   txt <- read.csv("datasets/abc.csv",stringsAsFactors = FALSE)
   txt<-txt %>% 
     select(lyrics, title,artist,sentiment)
   View(text)
   dataset <- rbind(txt,dataset)
   dataset <- dataset[1:150,]
   View(dataset)
   
   #separate train data and test data
   set.seed(1000)
   train<-sample(1:150,100,replace=FALSE)
   traindata<-dataset[train,]
   testdata <- dataset[1:80,]
   # testdata<-dataset[sample(1:150,80,replace=FALSE),]
   View(testdata)
   View(traindata)
   #train data according to C_SVM model
   #tune svm model for better accuracy
   #obj<-tune.svm(sentiment~.,data=traindata,gamma=seq(0.01,1,by=0.25),cost=seq(1,50,10))
   #summary(obj)
  

   
   # fit_default_linear_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="linear",nu=0.05)
   fit_default_linear<-svm(sentiment~.,data=traindata,kernel="linear")
   summary(fit_default_linear)
   
   #prediction on test data (svm-nu)
   
   pred_default_linear<-predict(fit_default_linear,testdata)
   
   testdata_linear<-testdata
   
   testdata_linear$sentiment<-pred_default_linear
   View(testdata_linear)
 
  
 })
  
 output$result <- renderTable(testdata_linear)
  output$words_frequency <- renderPlot({
    z=lyrics_dataset %>%
      unnest_tokens(word, lyrics) %>%
      distinct() %>%
      count(word, sort = TRUE) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n))
    showtext.begin()
    #View(z)
    g = ggplot(data = z, aes(x = word,y=n)) +
      geom_col()+
      ggtitle("Most Frequently used word") +
      theme_bw(base_size = 30, base_family = "dev")
    print(g)
    showtext.end()
  })
  
  
  output$songs_emo1 <- renderPlot({
    z=lyrics_nrc %>%
      filter(title %in% input$selectSong) %>%
      group_by(sentiment) %>%
      summarise(word_count = n()) %>%
      ungroup() %>%
      mutate(sentiment = reorder(sentiment, word_count))
    #View(z)
    showtext.begin()
    g=ggplot(data=z,aes(sentiment,word_count)) +
      geom_col()+
      ggtitle("Most Frequently used word") +
      guides(fill = FALSE) +
      theme_bw(base_size = 30, base_family = "dev")+
      ggtitle(input$selectSong) +
      coord_flip()
    print(g)
    showtext.end()
    
  })

  
  output$show_rec = DT::renderDataTable({subset(testdata, sentiment==input$selectEmo, title)})
  
  # output$overall_emo <- renderPlot({
  #   z=plot_words_1998 
  #   #View(z)
  #   showtext.begin()
  #   g=ggplot(data=z,aes(word, 1, label = word, fill = sentiment )) +
  #     geom_point(color = "transparent") +
  #     geom_label_repel(force = 1,nudge_y = .5,  
  #                      direction = "y",
  #                      box.padding = 0.05,
  #                      segment.color = "transparent",
  #                      size = 3) +
  #     facet_grid(~sentiment) +
  #     theme_lyrics() +
  #     theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
  #           axis.title.x = element_text(size = 6),
  #           panel.grid = element_blank(), panel.background = element_blank(),
  #           panel.border = element_rect("lightgray", fill = NA),
  #           strip.text.x = element_text(size = 9)) +
  #     theme_bw(base_size = 10, base_family = "dev")+
  #     xlab(NULL) + ylab(NULL) +
  #     ggtitle("NRC Sentiment") +
  #     coord_flip()
  #   print(g)
  #   showtext.end()
  # })

  output$analytical_accuracy <- renderPlot({
    # recall <- ggplot() +
    #   geom_line(data = g.recall, aes(x = x, y = value, color = func,group=1), size = 1)+
    #   geom_point()+
    #   theme_bw(base_size = 20, base_family = "dev")+
    #   labs(title = "Recall")
    # precision <- ggplot() +
    #   geom_line(data = g.precision, aes(x = x, y = value, color = func,group=1), size = 1)+
    #   geom_point()+
    #   theme_bw(base_size = 20, base_family = "dev")+
    #   labs(title = "Precision")
    # f1 <- ggplot() +
    #   geom_line(data = g.f1, aes(x = x, y = value, color = func,group=1), size = 1)+
    #   geom_point()+
    #   theme_bw(base_size = 20, base_family = "dev")+
    #   labs(title = "F1")
    linear <- ggplot() +
      geom_line(data = g.accuracy.l, aes(x = x, y = value, color = func,group=1), size = 1)+
      geom_point()+
      theme_bw(base_size = 20, base_family = "dev")
    
    sigmoid <- ggplot() +
      geom_line(data = g.accuracy.s, aes(x = x, y = value, color = func,group=1), size = 1)+
      geom_point()+
      theme_bw(base_size = 20, base_family = "dev")
    
    radial <- ggplot() +
      geom_line(data = g.accuracy.r, aes(x = x, y = value, color = func,group=1), size = 1)+
      geom_point()+
      theme_bw(base_size = 20, base_family = "dev")
    
    multiplot(linear, sigmoid, radial,  cols=2)
  })
  
  pred_new_radial
  
  output$out1 <- renderText(pred_new_radial)
  # output$out2 <- renderText(pred_default_radial)
  output$out3 <- renderText(pred_default_linear_nu)
  # output$out4 <- renderText(pred_default_poly)
  output$out5 <- renderText(pred_default_sigmoid_nu)
  
  output$testtable1 = DT::renderDataTable({table_new_radial[,2:4]})
  output$testtable2 = DT::renderDataTable({testdata_linear[,2:4]})
  output$testtable3 = DT::renderDataTable({testdata_sigmoid[,2:4]})
  
  output$sum1 <- renderPrint(summary(fit_new_radial))
  output$sum2 <- renderPrint(summary(fit_default_linear_nu))
  output$sum3 <- renderPrint(summary(fit_default_sigmoid_nu))
  # output$testtable4 = DT::renderDataTable({testdata4[,2:4]})
  # output$testtable5 = DT::renderDataTable({testdata5[,2:4]})
  # output$testtable6 = DT::renderDataTable({testdata6[,2:4]})
  # output$testtable7 = DT::renderDataTable({testdata7[,2:4]})
  
#//////////EOL//////////////EOL///////////////////EOL/////////////////EOL//////////////////
}

shinyApp(ui=ui, server=server)

# output$songemo1 <- renderPlot({
#   lyrics_nrc %>%
#     filter(title %in% input$selectSong) %>%
#     group_by(sentiment) %>%
#     summarise(word_count = n()) %>%
#     ungroup() %>%
#     mutate(sentiment = reorder(sentiment, word_count)) %>%
#     ggplot(aes(sentiment, word_count, fill = -word_count)) +
#     geom_col() +
#     guides(fill = FALSE) +
#     theme_minimal() + theme_lyrics() +
#     labs(x = NULL, y = "Word Count") +
#     ggtitle(input$selectSong) +
#     coord_flip()
# 
# })
# 
# output$songemo2 <- renderPlot({
#   
#   lyrics_tidy %>%
#     filter(title %in% input$selectSong) %>%
#     distinct(word) %>%
#     inner_join(getsentiments) %>%
#     ggplot(aes(x = word, fill = sentiment)) +
#     facet_grid(~sentiment) +
#     geom_bar() + #Create a bar for each word per sentiment
#     theme_lyrics() +
#     theme(panel.grid.major.x = element_blank(),
#           axis.text.x = element_blank()) + #Place the words on the y-axis
#     xlab(NULL) + ylab(NULL) +
#     ggtitle("NRC Words") +
#     coord_flip()
# })
# 
# output$emoword <- renderPlot({
#   plot_words_1998 %>%
#     ggplot(aes(word, 1, label = word, fill = sentiment )) +
#     geom_point(color = "transparent") +
#     geom_label_repel(force = 1,nudge_y = .5,  
#                      direction = "y",
#                      box.padding = 0.05,
#                      segment.color = "transparent",
#                      size = 3) +
#     facet_grid(~sentiment) +
#     theme_lyrics() +
#     theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
#           axis.title.x = element_text(size = 6),
#           panel.grid = element_blank(), panel.background = element_blank(),
#           panel.border = element_rect("lightgray", fill = NA),
#           strip.text.x = element_text(size = 9)) +
#     xlab(NULL) + ylab(NULL) +
#     ggtitle("NRC Sentiment") +
#     coord_flip()
# })
# 
# output$words_frequency <- renderPlot({
#   lyrics_dataset %>%
#     unnest_tokens(word, lyrics) %>%
#     anti_join(stop_words) %>%
#     distinct() %>%
#     filter(!word %in% undesirable_words) %>%
#     count(word, sort = TRUE) %>%
#     top_n(15) %>%
#     ungroup() %>%
#     mutate(word = reorder(word, n)) %>%
#     ggplot() +
#     geom_col(aes(word, n), fill = my_colors[2]) +
#     theme(legend.position = "none",
#           plot.title = element_text(hjust = 0.5)) +
#     xlab("") +
#     ylab("Song Count") +
#     theme(text=element_text(family="Samanata"))+
#     ggtitle("Most Frequently Used Words in Nepali lyrics") 
# })
# 
# output$emobar <- renderPlot({
#   lyrics_nrc %>%
#     group_by(sentiment) %>%
#     summarise(word_count = n()) %>%
#     ungroup() %>%
#     mutate(sentiment = reorder(sentiment, word_count)) %>%
#     #Use `fill = -word_count` to make the larger bars darker
#     g=ggplot(aes(sentiment, word_count, fill = -word_count)) +
#       geom_col() +
#       guides(fill = FALSE) + #Turn off the legend
#       theme_lyrics() +
#       labs(x = NULL, y = "Word Count") +
#       scale_y_continuous(limits = c(0, 1500)) + #Hard code the axis limit
#       ggtitle("Nepali NRC Sentiment") +
#       coord_flip()
#     print(g)
# })
# 
# output$emopic <- renderPlot({
#   plot(nrc_meme)
# })
# 
#Upload CSV
# output$csv_dataset<- renderTable({
#   setProgress("Working")
#   filetoRead=input$dataset
#   if(is.null(filetoRead)){
#     return()}
#   if(input$sel_dataset=="csv"){
#     read.table(filetoRead$datapath,sep=input$sel_upload,header = input$check_upload)}
# })
# 
# output$txt_dataset<-renderText({
#   if(input$sel_dataset=="txt"){
#     req(input$dataset)
#     inFile <- input$dataset
#     df <- readLines(inFile$datapath)
#     return(df)}
# })