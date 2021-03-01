
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)

library(geojsonio)
library(jsonlite)
library(RCurl)

source("sm_data.R")

# t <- list(
#   family = "Tahoma",
#   size = 14,
#   color = 'black')

# tx <- list(
#   family = "Tahoma",
#   size = 12,
#   color = 'black')

death_label <- list(visible=TRUE, 
                    title=
                      list(text = "Daily Deaths"
                      ))

case_label <- list(visible=TRUE, 
                   title=
                     list(text = "Daily Cases"
                     ))


shinyServer(function(input, output, session) {
  
  output$deathsplot <- renderPlotly({
    
    p <- dat %>% 
      plot_ly(source='deaths') %>% 
      add_trace(type='bar',  
                x=~Date, 
                y=~Daily_Deaths, 
                text = paste(dat$Date,"<br>",
                             dat$Daily_Deaths, "Deaths<br><i>", 
                             dat$Daily_Deaths_cum, "Cumulative deaths</i>" ),
                hoverinfo = "text",
                marker=list(color='lightgrey', width=1)) %>% 
      add_lines(x=~Date, y=~Daily_Deaths_ma, hoverinfo = "none") %>% 
      layout(showlegend = FALSE, yaxis=death_label)
    
    
    # make event lines
    event_cols <- c('#a17403', '#187818', '#3b403b','#8a0303', '#363ed9')
    tmp <- dat %>% dplyr::filter(!is.na(dat$Types_EO))
    tmp_data <- tmp %>% mutate(dummy = max(dat$Daily_Deaths, na.rm=TRUE),
                               dummy2 = max(dat$Daily_Deaths, na.rm=TRUE) * 1.03)
    for (i in 1:nrow(tmp_data)) {
      line_color <- event_cols[as.numeric(tmp_data$Types_EO[i])]
      p <- p %>% 
        add_segments(x = tmp_data$Date[i], y=0, 
                     xend=tmp_data$Date[i], yend=tmp_data$dummy[1],
                     text = paste0(tmp_data$Date[i],"<br>",tmp_data$Types_EO[i]), 
                     hoverinfo = 'text',
                     opacity=0.75,
                     line=list(color=line_color, dash='dash', width=1))
      p <- p %>% 
        add_markers(x = tmp_data$Date[i], y = tmp_data$dummy2[i], 
                    text = paste0(tmp_data$Date[i],"<br>",tmp_data$Types_EO[i]), 
                    hoverinfo = 'text', 
                    type='scatter', mode='markers', marker = list(color=line_color))
    }
    p %>% 
      event_register("plotly_click")
    
  })
  
  
  
  
  output$casesplot <- renderPlotly({
    
    p <- dat %>% 
      plot_ly(source='cases') %>% 
      add_trace(type='bar',  
                x=~Date, 
                y=~Daily_Cases, 
                text = paste(dat$Date,"<br>",
                             dat$Daily_Cases, "Cases<br><i>", 
                             dat$Daily_Cases_cum, "Cumulative cases</i>" ),
                hoverinfo = "text",
                marker=list(color='lightgrey', width=1), showlegend=F) %>% 
      add_lines(x=~Date, y=~Daily_Cases_ma, hoverinfo = "none", showlegend=F) %>% 
      layout(showlegend = FALSE, yaxis=case_label)
    # 
    # make event lines
    event_cols <- c('#a17403', '#187818', '#3b403b','#8a0303', '#363ed9')
    tmp <- dat %>% dplyr::filter(!is.na(dat$Types_EO))
    tmp_data <- tmp %>% mutate(dummy = max(dat$Daily_Cases, na.rm=TRUE),
                               dummy2 = max(dat$Daily_Cases, na.rm=TRUE) * 1.03)
    for (i in 1:nrow(tmp_data)) {
      line_color <- event_cols[as.factor(tmp_data$Types_EO[i])]
      p <- p %>% 
        add_segments(x = tmp_data$Date[i], y=0, 
                     xend=tmp_data$Date[i], yend=tmp_data$dummy[1],
                     text = paste0(tmp_data$Date[i],"<br>",tmp_data$Types_EO[i]), 
                     hoverinfo = 'text',
                     opacity=0.75,
                     line=list(color=line_color, dash='dash', width=1), showlegend=F)
      p <- p %>% 
        add_markers(x = tmp_data$Date[i], y = tmp_data$dummy2[i], 
                    text = paste0(tmp_data$Date[i],"<br>",tmp_data$Types_EO[i]), 
                    hoverinfo = 'text', 
                    type='scatter', mode='markers', marker = list(color=line_color), 
                    showlegend=F)
    }
    p %>% 
      event_register("plotly_click")
    
    
  })
  
  
  observeEvent(event_data("plotly_click", source = "deaths"), {
    d = event_data("plotly_click", source = "deaths")
    cat("from death plot", d$x, '\n')
    
    policy_txt = ""
    if (d$pointNumber[1] %in% c(0,1)) {
      x <- d$x[1]
      tmp <- subset(dat, Date == x)
      policy_txt = tmp$Details_1[1]
    }
    
    updateTextInput(session, "hidden", value = sprintf("XXXdeaths|%s|%s", d$x, policy_txt))
    
  })
  
  observeEvent(event_data("plotly_click", source = "cases"), {
    d = event_data("plotly_click", source = "cases")
    cat("from cases plot", d$x, '\n')
    
    policy_txt = ""
    if (d$pointNumber[1] %in% c(0,1)) {
      x <- d$x[1]
      tmp <- subset(dat, Date == x)
      policy_txt = tmp$Details_1[1]
    }
    
    updateTextInput(session, "hidden", value = sprintf("XXXcases|%s|%s", d$x, policy_txt))
  })
  
  
  output$click <- renderText({
    clicked_info <- input$hidden
    clicked_comps <- strsplit(clicked_info, "\\|")[[1]]
    if (is.na(clicked_comps[3]) || is.null(clicked_comps[3]))
      return("Click on an event point to see the details.")
    else 
      date<-strptime(clicked_comps[2], "%Y-%m-%d")
    datenice <- format(date, "%B %d, %Y")
      return(paste0(datenice,
                    "\n",
                    clicked_comps[3], 
                    sep="\n"))
  })
  
  
  observeEvent(input$tabs, {
    val = input$hidden
    print(paste0("this is", val))
    if (input$tabs == 'Deaths')
      newval <- gsub("XXXcases", "XXXdeaths", val)
    else 
      newval <- gsub("XXXdeaths", "XXXcases", val)
    updateTextInput(session, "hidden", value = newval)
  })


  
  output$map1 <- renderLeaflet({
    cat("We're here for map1\n")
    
    clicked_info <- input$hidden
    clicked_comps <- strsplit(clicked_info, "\\|")[[1]]
    clicked_plot = clicked_comps[1]
    clicked_date = clicked_comps[2]
    
    date_comps <- strsplit(clicked_date, "-")[[1]]
    
    if (length(date_comps) != 3) return(NULL)
    
    mm <- month.name[as.integer(date_comps[2])]
    col_name <- sprintf("%s_%d_%s", mm, as.integer(date_comps[3]), date_comps[1])
    cat(col_name, '\n')
    
    if (col_name %in% names(dat_cases)==TRUE){
    
    todays_idx = which(names(dat_cases) == col_name)
    yesterday_idx = todays_idx - 1
  
    
    if (clicked_plot == 'XXXcases') {
      dat_to_use <- dat_cases
      dat_to_use$pdat <- dat_cases[[todays_idx]] - dat_cases[[yesterday_idx]]
      dat_to_use$pdat[ dat_to_use$pdat < 0] <- 0
      
      if (!all(dat_to_use$pdat == 0)) {
        dat_to_use$pdat <- round(100*dat_to_use$pdat/sum(dat_to_use$pdat))
      }
      
      pal = colorNumeric("YlGn",dat_to_use$pdat, alpha = TRUE)
      #pal <- myQuantile(dat_to_use$pdat, 5, "YlGn")
      #pal = colorBin("OrRd", jitter(dat_to_use[[col_name]]), bins=9)
    } else {
      dat_to_use <- dat_deaths
      dat_to_use$pdat <- dat_deaths[[todays_idx]] - dat_deaths[[yesterday_idx]]
      dat_to_use$pdat[ dat_to_use$pdat < 0] <- 0
      
      if (!all(dat_to_use$pdat == 0)) {
        dat_to_use$pdat <- round(100*dat_to_use$pdat/sum(dat_to_use$pdat))
      }
      
      pal = colorNumeric("Reds",dat_to_use$pdat, alpha = TRUE)
      #pal <- myQuantile(dat_to_use$pdat, 5, "Reds")
      #pal = colorBin("Reds", jitter(dat_to_use[[col_name]]), bins=9)
    }
   
    # if (col_name %in% names(dat_to_use)==TRUE){
    
    legdate<-strptime(col_name, "%B_%d_%Y")
    legdatenice <- format(legdate, "%b/%d/%Y")
    
    m <- leaflet(dat_to_use) %>%
      addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
      setView(-72.699997, 41.599998, 8) %>%
      addPolygons(stroke=TRUE, weight=1, color='grey', fillOpacity = 0.8, 
                  popup = paste0("<b>Town:</b> ",dat_to_use$TOWN, "<br>","<b>Percentage:</b> ", dat_to_use[["pdat"]]),
                  smoothFactor = 0.2, fillColor = pal(dat_to_use[["pdat"]])) %>% 
      addLegend(pal = pal, values = dat_to_use[["pdat"]], opacity=1, 
                title = paste0(legdatenice,"<br>","<center>" ,"(in %)")) 
    m
    
    }else{
      m <- leaflet(dat_cases) %>%
        addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
        setView(-72.699997, 41.599998, 8) %>%
        addPolygons(stroke=TRUE, weight=1, color='grey', fillOpacity = 0.8) %>%
        addLegend(labels="No Data Available", title="", col="grey")
    
      m
      
    }
  })
  
session$onSessionEnded(stopApp)

})

