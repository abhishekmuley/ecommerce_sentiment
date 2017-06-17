library(shinydashboard)
library(shiny)
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(dismo)
library(ggmap)
library(maps)
library(XML)
library(RJSONIO)
library(httr)
library(xml2)
library(gtrendsR)
library(base64enc)




#Ecommerce 
pacman::p_load(XML, dplyr, stringr, rvest, audio)








#twitter API Outh

api_key <- XXXXXXXXXXXXXXXXXXXXXX"

api_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"  

setup_twitter_oauth(api_key,api_secret)




options(shiny.error=recover)





shinyApp(
  
  
  ui = dashboardPage(
    
    dashboardHeader(title = "Analytico"),
    
    
    
    dashboardSidebar(
      
      
      sidebarMenu(
        
        menuItem("Home Page", tabName = "homepage", icon = icon("home")),
        
        menuItem("Text Analysis", icon = icon("twitter"),
                 
                 # textInput("prod_code", "", "Keyword"),
                 sidebarSearchForm("prod_code", "Twitter_Sentiment_analysis", label = "Search...",
                                   icon = shiny::icon("search")),
                 
                 #actionButton("Twitter_Sentiment_analysis","Generate Twitter Sentiment "),
                 
                 menuItem("Twitter Sentiment", tabName = "Twitter_sentiment", icon = icon("twitter")),
                 
                 menuItem("Sentiment Cloud", tabName = "wet", icon = icon("twitter")),
                 
                 menuItem("Sentiment Polarity", tabName = "war", icon = icon("twitter")),
                 
                 menuItem("Related Wordcloud", tabName = "Twitter_Wordcloud", icon = icon("twitter")),
                 
                 menuItem("Twitter Heatmap", tabName = "Twitter_Heatmap", icon = icon("twitter"))
                 
                 
        ),
        
        menuItem("E-Commerce", tabName = "E_Commerce", icon = icon("amazon"),
                 
                 # textInput("pcode", "Ecommerce", "Product Code"),
                 
                 sidebarSearchForm("pcode", "Ecommerce_Reviews", label = "Search...",
                                   icon = shiny::icon("search")),
                 
                 #actionButton("Ecommerce_Reviews","Ecommerce Reviews"),
                 
                 menuItem("Amazon Sentiment", tabName = "E_Commerce", icon = icon("amazon")),
                 
                 menuItem("Rating Plot",tabName = "RatingPlot", icon = icon("amazon")),
                 
                 menuItem("Polarity Text",tabName = "PolarityText", icon = icon("amazon"))
                 
        ),
        menuItem("About Us",tabName = "aboutus", icon = icon("aboutus")),
        
        menuItem("Acknowledgement",tabName = "acknowledgement", icon = icon("acknowledgement"))
      )
      
    ),
    
    
    dashboardBody(
      
      fluidPage(fluidRow
                
                (align="center",
                
                column(8, align="center", offset = 2,
                       
                       tags$head(tags$script(src = "message-handler.js")),                                 
                       tags$style(type="text/css", "#string { height: 50; width: 100%; text-align:center; font-size: 30;}"),
                       tags$style(type="text/css", "#plot1_dl { width:100; vertical-align:50%}"),  
                       
                       tabItems(
                         
                         tabItem(tabName = "aboutus", 
                                 h3("About Us"),
                                 wellPanel(
                                   fluidRow( 
                                     
                                     column(12, offset = 0.5,
                                            
                                            p(" ")
                                            
                                            ),
                                     column(12, offset = 4,
                                            tags$p(
                                              tags$p("Rohit Shetty"), 
                                              tags$p("Abhishek Muley"), 
                                              tags$p("Jeet Patel"),
                                              tags$p("Keyura Mujumdar")
                                            ))
                                     )
                                   
                                     )
                                
                                 
                         ),
                         
                         tabItem(tabName = "acknowledgement", 
                                 h3("Acknowledgement"),
                                 
                                 
                                 
                                 
                                 wellPanel(
                                   fluidRow( 
                                     
                                     column(12, offset = 0.5,
                                            
                                            p("We take this opportunity to our thank Project Guide Prof. R. C. Salunkhe, Asst.
                                              Professor, Sinhgad Institute of Technology and Science, Narhe for his exemplary guid-
                                              ance, monitoring and constant encouragement throughout the course of this project.
                                              We would specially thank Prof. J. B. Karande,Project Co-ordinator, for his cordial
                                              support as he gave the permission to use all required equipment and the necessary
                                              material to complete the project. We sincerely thank Dr. A. P. Adsul, Head of
                                              Department, Information Technology, Sinhgad Institute of Technology and Science,
                                              Narhe and Dr. S. N. Mali, Principal, Sinhgad Institute of Technology and Science,
                                              Narhe, for their valuable inputs ")
                                            
                                            )
                                    
                                            )
                                   
                                     )
                                 
                                 
                                     ),
                         
                         
                         tabItem(tabName = "homepage", 
                                 h2("WELCOME"),
                                 wellPanel(
                                   fluidRow( 
                                     
                                     column(12, offset = 0.5,
                                            
                                            p("
                      Web 2.0 Analytics Search Engine that takes
                      a query as input and outputs a detailed analysis report of that keyword using data
                      from various dynamic sources such as Twitter and E-commerce website Amazon."),

                      p("The name Web 2.0 indicates an improvisation over the previous Web 1.0, which was
                      the static web. During the early years of the internet, the data 
                      ow was mostly unidi-
                      rectional, meaning, the information was visible to the end user but he/she could not
                      interact with it. The emergence of social media, e-commerce and such technologies
                      revolutionized the way we use internet. The information 
                      ow became more bi direc-
                      tional, where users and the computers interacted with each other. This gave rise to
                      Web 2.0 which can be dened as the version of the internet which is more dynamic
                      and depends on user generated data.
                      Some examples of Web 2.0 are Social media websites and E-commerce. Social media
                      generates around thousands of terabytes of data daily. All this data is user generated
                      data which could be text, images and videos. E-commerce too, generates massive
                      amounts of data which comes under the category of user generated data. Informa-
                      tion such as user preferences, user activities, locations and even sentiments can be
                      gathered from Web 2.0 applications.  ")
                                            
                                     )))
                                 
                         ),
                         # Twitter sentiment tab content
                         
                         tabItem(tabName = "Twitter_sentiment",
                                 
                                 h3("Twitter Sentiment"),
                                 br(),
                                 
                                 #textInput("prod_code", h2("Twitter Sentiment"), "Keyword / Product Code"),
                                 # actionButton("Twitter_Sentiment_analysis","Generate Twitter Sentiment "),
                                 
                                 plotOutput("tsentiPlot",width = "800px", height = "700px")
                                 
                                 
                                 
                                 
                         ),#End Twitter sentiment tab content
                         
                         
                         
                         tabItem(tabName = "wet",
                                 
                                 h3("Twitter Sentiment Cloud"),
                                 br(),
                                 
                                 #textInput("prod_code", h2("Twitter Sentiment"), "Keyword / Product Code"),
                                 # actionButton("Twitter_Sentiment_analysis","Generate Twitter Sentiment "),
                                 
                                 plotOutput("wet",width = "800px", height = "700px")
                                 
                                 
                                 
                                 
                         ),#End Twitter sentiment tab content
                         
                         
                         tabItem(tabName = "war",
                                 
                                 h3("Sentiment Polarity"),
                                 br(),
                                 
                                 #textInput("prod_code", h2("Twitter Sentiment"), "Keyword / Product Code"),
                                 # actionButton("Twitter_Sentiment_analysis","Generate Twitter Sentiment "),
                                 
                                 plotOutput("war",width = "800px", height = "700px")
                                 
                                 
                                 
                                 
                         ),#End Twitter sentiment tab content
                         
                         
                         # Twitter Wordcloud tab content
                         
                         tabItem(tabName = "Twitter_Wordcloud",
                                 
                                 h3("Twitter Wordcloud"),
                                 br(),
                                 
                                 #actionButton("Twitter_wordcloud","Twitter WordCloud"),
                                 
                                 plotOutput("cloudPlot",width = "800px", height = "700px")
                         ),
                         
                         
                         
                         
                         
                         
                         
                         
                         # Twitter Heatmap tab content
                         
                         tabItem(tabName = "Twitter_Heatmap",
                                 
                                 h3("Twitter HeatMap"),
                                 br(),
                                 
                                 #  actionButton("Twitter_Heatmap","Twitter Heatmap"),
                                 plotOutput("heatPlot",width = "800px", height = "700px")
                         ),
                         
                         
                         
                         
                         # Google Trends tab content
                         
                         # tabItem(tabName = "Google_Trends"
                         
                         
                         # ),
                         
                         
                         # E-Commerce tab content
                         
                         tabItem(tabName = "E_Commerce",
                                 
                                 h3("Amazon Product Sentiment Histogram"),
                                 br(),
                                 
                                 #  textInput("pcode", "Ecommerce", "Product Code"),
                                 #  actionButton("Ecommerce_Reviews","Ecommerce Reviews"),
                                 
                                 
                                 plotOutput("sentiPlot",width = "800px", height = "700px")
                                 # plotOutput("Ecommerce_Reviews$starPlot"),
                                 #  plotOutput("Ecommerce_Reviews$textPlot")
                                 
                                 
                         ),
                         
                         
                         tabItem(tabName = "RatingPlot",
                                 
                                 h3("Amazon Product Rating Plot"),
                                 br(),
                                 
                                 # textInput("pcode", "Ecommerce", "Product Code"),
                                 #actionButton("Ecommerce_Reviews","Ecommerce Reviews"),
                                 
                                 
                                 #  plotOutput("sentiPlot")
                                 plotOutput("starPlot",width = "800px", height = "700px")
                                 #  plotOutput("Ecommerce_Reviews$textPlot")
                                 
                                 
                         ),
                         
                         tabItem(tabName = "PolarityText",
                                 
                                 h3("Amazon Polarity Text"),
                                 br(),
                                 
                                 # textInput("pcode", "Ecommerce", "Product Code"),
                                 # actionButton("Ecommerce_Reviews","Ecommerce Reviews"),
                                 
                                 
                                 #  plotOutput("sentiPlot")
                                 #plotOutput("Ecommerce_Reviews$starPlot")
                                 plotOutput("textPlot")
                                 
                                 
                         )
                         
                         
                         
                         
                         
                         
                                     )# ENd Tab Items
                       # mainPanel(
                       #  plotOutput("sentiPlot")
                       #)
                       
                                     )# END column
                
                
                                   )#end align
                
                                 )#END fluid page
      
      
      
      
      
      
                )#End Dashboard body
    
    
    
    
    
    
    
    
                ),#dashboard page
  
  
  server = function(input, output, session) {
    
    
    
    
    #Twitter Sentiment Analysis
    
    observeEvent(input$Twitter_Sentiment_analysis, {
      
      
      # harvest some tweets
      some_tweets = searchTwitter(input$prod_code, n = 150, lang="en")
      
      # get the text
      some_txt = sapply(some_tweets, function(x) x$getText())
      
      # remove retweet entities
      some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
      # remove at people
      some_txt = gsub("@\\w+", "", some_txt)
      # remove punctuation
      some_txt = gsub("[[:punct:]]", "", some_txt)
      # remove numbers
      some_txt = gsub("[[:digit:]]", "", some_txt)
      # remove html links
      some_txt = gsub("http\\w+", "", some_txt)
      # remove unnecessary spaces
      some_txt = gsub("[ \t]{2,}", "", some_txt)
      some_txt = gsub("^\\s+|\\s+$", "", some_txt)
      
      # define "tolower error handling" function 
      try.error = function(x)
      {
        # create missing value
        y = NA
        # tryCatch error
        try_error = tryCatch(tolower(x), error=function(e) e)
        # if not an error
        if (!inherits(try_error, "error"))
          y = tolower(x)
        # result
        return(y)
      }
      # lower case using try.error with sapply 
      some_txt = sapply(some_txt, try.error)
      # remove NAs in some_txt
      some_txt = some_txt[!is.na(some_txt)]
      names(some_txt) = NULL
      
      # classify emotion
      class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
      # get emotion best fit
      emotion = class_emo[,7]
      # substitute NA's by "unknown",
      emotion[is.na(emotion)] = "unknown"
      
      # classify polarity
      class_pol = classify_polarity(some_txt, algorithm="bayes")
      # get polarity best fit
      polarity = class_pol[,4]
      
      
      # data frame with results
      sent_df = data.frame(text=some_txt, emotion=emotion,
                           polarity=polarity, stringsAsFactors=FALSE)
      
      # sort data frame
      sent_df = within(sent_df,
                       emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
      
      output$tsentiPlot<-renderPlot({
        g<-ggplot(sent_df, aes(x=emotion)) +
          geom_bar(aes(y=..count.., fill=emotion)) +
          scale_fill_brewer(palette="Dark2") +
          labs(x="emotion categories", y="number of tweets") 
        print(g)
        
        
      })
      
      
      output$wet<-renderPlot({
        # separating text by emotion
        emos = levels(factor(sent_df$emotion))
        nemo = length(emos)
        emo.docs = rep("", nemo)
        for (i in 1:nemo)
        {
          tmp = some_txt[emotion == emos[i]]
          emo.docs[i] = paste(tmp, collapse=" ")
        }
        
        # remove stopwords
        emo.docs = removeWords(emo.docs, stopwords("english"))
        # create corpus
        corpus = Corpus(VectorSource(emo.docs))
        tdm = TermDocumentMatrix(corpus)
        tdm = as.matrix(tdm)
        colnames(tdm) = emos
        
        # comparison word cloud
        sc <- comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                               scale = c(3,.5), random.order = FALSE, title.size = 1.5)
        
        print(sc)
        
        
      })
      
      
      output$war<-renderPlot({
        # plot distribution of polarity
        pr<-ggplot(sent_df, aes(x=polarity)) +
          geom_bar(aes(y=..count.., fill=polarity)) +
          scale_fill_brewer(palette="RdGy") +
          labs(x="polarity categories", y="number of tweets") 
        # opts(title = "Sentiment Analysis of Tweets about Starbucks\n(classification by polarity)",
        # plot.title = theme_text(size=12)
        print(pr)
        
      })
    })#end Twitter sentiment
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #START Twitter WORDcLOUD
    
    observeEvent(input$Twitter_Sentiment_analysis, {
      
      
      
      tweets <- searchTwitter(input$prod_code, n=100)  
      tweets.text = laply(tweets,function(t)t$getText())
      
      
      
      clean.text <- function(some_txt)
      {
        some_txt = gsub("&amp", "", some_txt)
        
        some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
        
        some_txt = gsub("@\\w+", "", some_txt)
        
        some_txt = gsub("[[:punct:]]", "", some_txt)
        
        some_txt = gsub("[[:digit:]]", "", some_txt)
        
        some_txt = gsub("http\\w+", "", some_txt)
        
        some_txt = gsub("[ t]{2,}", "", some_txt)
        
        some_txt = gsub("^\\s+|\\s+$", "", some_txt)
        
        # define "tolower error handling" function
        
        try.tolower = function(x)
          
        {
          
          y = NA
          
          try_error = tryCatch(tolower(x), error=function(e) e)
          
          if (!inherits(try_error, "error"))
            
            y = tolower(x)
          
          return(y)
          
        }
        
        some_txt = sapply(some_txt, try.tolower)
        
        some_txt = some_txt[some_txt != ""]
        
        names(some_txt) = NULL
        
        return(some_txt)
        
      }
      
      
      
      clean_text = clean.text(tweets.text)
      
      tweet_corpus = Corpus(VectorSource(clean_text))
      
      tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE, removeNumbers = TRUE, tolower = TRUE))
      
      require(plyr)
      
      m = as.matrix(tdm)
      
      word_freqs = sort(rowSums(m), decreasing=TRUE) 
      
      dm = data.frame(word=names(word_freqs), freq=word_freqs) 
      
      
      
      
      output$cloudPlot<-renderPlot({
        g<- wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) 
        print(g)
      })
    })# END Twitter WORDCLOUD
    
    
    
    
    
    
    
    
    
    
    
    #heat map
    
    observeEvent(input$Twitter_Sentiment_analysis, {
      
      searchResults <- searchTwitter(input$prod_code, n = 30)  # Gather Tweets 
      tweetFrame <- twListToDF(searchResults)  # Convert to a nice dF
      
      userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
      userFrame <- twListToDF(userInfo)  # Convert to a nice dF
      
      locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info
      
      locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
      #approximate lat/lon from textual location data.
      with(locations, plot(lon, lat))
      
      worldMap <- map_data("world")  # Easiest way to grab a world map shapefile
      
      zp1 <- ggplot(worldMap)
      zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                             colour = gray(2/3), lwd = 1/3)
      zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                              aes(x = lon, y = lat),
                              colour = "RED", alpha = 1/2, size = 1)
      zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
      zp1 <- zp1 + theme_minimal()  # Drop background annotations
      
      
      output$heatPlot<-renderPlot({
        print(zp1)
        
        
        
      })
    })
    #end heat map
    
    
    
    
    
    
    
    
    
    
    
    # Ecommerce
    
    observeEvent(input$Ecommerce_Reviews, {
      
      
      #Remove all white space
      trim <- function (x) gsub("^\\s+|\\s+$", "", x)
      
      #prod_code = "B01D4EYNQA"
      prod_code = input$pcode
      url <- paste0("https://www.amazon.in/dp/", prod_code)
      doc <- read_html(url)
      
      #obtain the text in the node, remove "\n" from the text, and remove white space
      prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
      # prod
      
      #Source funtion to Parse Amazon html pages for data
      #source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")
      
      
      
      
      
      #Parse Amazon html pages for data
      amazon_scraper <- function(doc, reviewer = T, delay = 0){
        
        if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
        pacman::p_load_gh("trinker/sentimentr")
        pacman::p_load(RCurl, XML, dplyr, stringr, rvest, audio)
        
        sec = 0
        if(delay < 0) warning("delay was less than 0: set to 0")
        if(delay > 0) sec = max(0, delay + runif(1, -1, 1))
        
        #Remove all white space
        trim <- function (x) gsub("^\\s+|\\s+$", "", x)
        
        title <- doc %>%
          html_nodes("#cm_cr-review_list .a-color-base") %>%
          html_text()
        
        author <- doc %>%
          html_nodes(".review-byline .author") %>%
          html_text()
        
        date <- doc %>%
          html_nodes("#cm_cr-review_list .review-date") %>%
          html_text() %>% 
          gsub(".*on ", "", .)
        
        ver.purchase <- doc%>%
          html_nodes(".review-data.a-spacing-mini") %>%
          html_text() %>%
          grepl("Verified Purchase", .) %>%
          as.numeric()
        
        format <- doc %>% 
          html_nodes(".review-data.a-spacing-mini") %>% 
          html_text() %>%
          gsub("Color: |\\|.*|Verified.*", "", .)
        #if(length(format) == 0) format <- NA
        
        stars <- doc %>%
          html_nodes("#cm_cr-review_list  .review-rating") %>%
          html_text() %>%
          str_extract("\\d") %>%
          as.numeric()
        
        comments <- doc %>%
          html_nodes("#cm_cr-review_list .review-text") %>%
          html_text() 
        
        helpful <- doc %>%
          html_nodes(".cr-vote-buttons .a-color-secondary") %>%
          html_text() %>%
          str_extract("[:digit:]+|One") %>%
          gsub("One", "1", .) %>%
          as.numeric()
        
        if(reviewer == T){
          
          rver_url <- doc %>%
            html_nodes(".review-byline .author") %>%
            html_attr("href") %>%
            gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
            gsub("/gp/pdp/profile/", "", .) %>%
            paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
          
          #average rating of past 10 reviews
          rver_avgrating_10 <- rver_url %>%
            sapply(., function(x) {
              read_html(x) %>%
                html_nodes(".small span img") %>%
                html_attr("title") %>%
                gsub("out of.*|stars", "", .) %>%
                as.numeric() %>%
                mean(na.rm = T)
            }) %>% as.numeric()
          
          rver_prof <- rver_url %>%
            sapply(., function(x) 
              read_html(x) %>%
                html_nodes("div.small, td td td .tiny") %>%
                html_text()
            )
          
          rver_numrev <- rver_prof %>%
            lapply(., function(x)
              gsub("\n  Customer Reviews: |\n", "", x[1])
            ) %>% as.numeric()
          
          rver_numhelpful <- rver_prof %>%
            lapply(., function(x)
              gsub(".*Helpful Votes:|\n", "", x[2]) %>%
                trim()
            ) %>% as.numeric()
          
          rver_rank <- rver_prof %>%
            lapply(., function(x)
              gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
                removePunctuation() %>%
                trim()
            ) %>% as.numeric()
          
          df <- data.frame(title, date, ver.purchase, format, stars, comments, helpful,
                           rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, rver_rank, stringsAsFactors = F)
          
        } else df <- data.frame(title, author, date, ver.purchase, format, stars, comments, helpful, stringsAsFactors = F)
        
        return(df)
      }
      
      
      
      
      
      
      
      
      
      pages <- 3
      
      reviews_all <- NULL
      for(page_num in 1:pages){
        url <- paste0("http://www.amazon.in/product-reviews/",prod_code,"/?pageNumber=", page_num)
        doc <- read_html(url)
        
        reviews <- amazon_scraper(doc, reviewer = F, delay = 2)
        reviews_all <- rbind(reviews_all, cbind(prod, reviews))
      }
      
      pacman::p_load_gh("trinker/sentimentr")
      
      sent_agg <- with(reviews_all, sentiment_by(comments))
      head(sent_agg)
      
      par(mfrow=c(1,2))
      
      
      
      output$sentiPlot<-renderPlot({
        g<-#with(reviews_all, hist(stars))
          with(sent_agg, hist(ave_sentiment))
        print(g)
      })
      
      output$starPlot<-renderPlot({
        e<-with(reviews_all, hist(stars))
        #with(sent_agg, hist(ave_sentiment))
        
        
        
        print(e)
      })
      
      output$textPlot<-renderPlot({
        best_reviews <- slice(reviews_all, top_n(sent_agg, 50, ave_sentiment)$element_id)
        worst_reviews <- slice(reviews_all, top_n(sent_agg, 50, -ave_sentiment)$element_id)
        f<-  with(worst_reviews, sentiment_by(comments)) %>% highlight()
        f<-with(best_reviews, sentiment_by(comments)) %>% highlight()
        a("test", href="Documents/polarity.html", target="_blank")  
      })
    })#END ECOMMERCE 
    
    
    
    #google trends
    
    
    
    
  }
  
    )
