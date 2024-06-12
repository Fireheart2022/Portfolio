library("tidyverse")
library(rvest)
library(polite)
library(highcharter)
library(shiny)
library(shinycssloaders)
library(bs4Dash)
library(fresh)
library(magrittr)
library(gt)
library(gtExtras)
library(shinyWidgets)
library(formattable)
###########################theme ###################################
rm(list=ls())
theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF",
    text_light = "#272c30"
  ),
  bs4dash_layout(
    main_bg = "#353c42"
  ),
  bs4dash_sidebar_light(
    bg = "#272c30",
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30",
    submenu_color = "#FFF",
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "red", danger = "#BF616A", light = "#272c30"
  ),
  bs4dash_color(
    gray_900 = "#FFF", white = "#272c30"
  )
)

########gt##########################datasets#####################################
low_case_country<-str_to_lower(c("Argentina",
                                  "Australia",
                                  "Austria",
                                  "Bahamas",
                                  "Bahrain",
                                  "Bangladesh",
                                  "Belgium",
                                  "Bolivia",
                                  "Brazil",
                                  "Bulgaria",
                                  "Canada",
                                  "Chile",
                                  "Canada",
                                  "Colombia",
                                  "Croatia",
                                  "Cyprus",
                                  "Denmark",
                                  "Ecuador",
                                  "Egypt",
                                  "Estonia",
                                  "Finland",
                                  "France",
                                  "Germany",
                                  "Greece",
                                  "Guadeloupe",
                                  "Guatemala",
                                  "Honduras",
                                  "Hungary",
                                  "Iceland",
                                  "India",
                                  "Indonesia",
                                  "Ireland",
                                  "Israel",
                                  "Italy",
                                  "Jamaica",
                                  "Japan",
                                  "Jordan",
                                  "Kenya",
                                  "Kuwait",
                                  "Latvia",
                                  "Lebanon",
                                  "Lithuania",
                                  "Luxembourg",
                                  "Malaysia",
                                  "Maldives",
                                  "Malta",
                                  "Martinique",
                                  "Mauritius",
                                  "Mexico",
                                  "Morocco",
                                  "Netherlands",
                                  "Nicaragua",
                                  "Nigeria",
                                  "Norway",
                                  "Oman",
                                  "Pakistan",
                                  "Panama",
                                  "Paraguay",
                                  "Peru",
                                  "Philipines",
                                  "Poland",
                                  "Portugal",
                                  "Qatar",
                                  "Romania",
                                  "Serbia",
                                  "Singapore",
                                  "Slovakia",
                                  "Slovenia",
                                  "Spain",
                                  "Sweden",
                                  "Switzerland",
                                  "Thailand",
                                  "Turkey",
                                  "Vietnam"))# end string

shinyApp(
ui = dashboardPage(
  freshTheme=theme,
  dashboardHeader(
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        "data-toggle" = "dropdown",
        div(tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Netflix_2015_logo.svg/2560px-Netflix_2015_logo.svg.png", 
                 height = 30, style = "margin-top: -7px;", align="left")),
        tags$span(class = "caret")
      )),
    
    div(tags$img(src="https://icones.pro/wp-content/uploads/2021/06/icone-github-rouge.png",
             height=20,style="magin-top: -7px; text-align:left")),
    dropdown( 
      messageItem(
        from = "GitHub",
        message = "Script of the code",
        icon=icon("github"),
        href=""
      )
      )
    ,
    title = bs4DashBrand(
      title = "NETFLIX",
      color = "navy",
      href = "https://www.netflix.com",
      image = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Netflix_2015_logo.svg/2560px-Netflix_2015_logo.svg.png"
    , opacity=1) # end title
  ), # end header
  dashboardSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "Top 10 by country",
        tabName='page1'
      ),
      bs4SidebarMenuItem(
        "Global Top 10",
        tabName='page2'
      )
      
  )# end menu
  ), # end sidebar menu
  dashboardBody(
    bs4TabItems(
      bs4TabItem(
        tabName="page1",
    fluidRow(
    box(width=3,
    selectInput("country",label="Select a country",choices=c(str_to_title(low_case_country))
              ), #end input
    
    selectInput(inputId="type",label="Select a type",choices=c("Films","TV")),# end select input

    airDatepickerInput(
      inputId = "date",
      label = "Select a date:",
      placeholder = today(),
      multiple = 5, 
      clearButton = TRUE
    ),#end date
    actionButton(inputId="go",label="GO!", icon = icon("play"),
                  class = "btn-lg btn-success")
    ), # end box 
    
    box(width=9, height=px(500),
      gt_output(outputId = "tbl")%>%withSpinner(color="red")
        ) #end box 2 
    ) # end FluidRow
    
      )# end tab item 1
    ,
    bs4TabItem(
      tabName = "page2",
      fluidRow(
        box(width=12,
            gt_output(outputId = "global_table")%>%withSpinner(color="red")
        )# end box 
        
      )# end fluidRow
    ) #end tab item 2 
    )# end tab items
  )# end dashboard Body
), # end dashboard page
server = function(input, output) {
  
  df <- eventReactive(input$go, {
    
    url =paste0("https://www.netflix.com/tudum/top10?week=",input$date)
    name = url%>% 
      read_html(.)%>%
      html_nodes(".tbl-cell-name")%>% html_text(trim=TRUE)
    
    weeks =url%>% 
      read_html(.)%>%
      html_nodes(".wk-number")%>% html_text(trim=TRUE)
    
    imgs = url%>%
      read_html()%>%
      html_nodes(".banner-image")%>%html_attr("src")
    
    rank =url%>% 
      read_html(.)%>%
      html_nodes(".tbl-cell-rank")%>% html_text(trim=TRUE)%>%as.numeric()
    
    hours_viewed =url%>% 
      read_html(.)%>%
      html_nodes(".tbl-cell-hours .inline-block")%>% html_text(trim=TRUE)%>%gsub(pattern=",",replacement="")%>%
    as.numeric()%>%
      formattable::accounting(digits=0)
    
    views =url%>% 
      read_html(.)%>%
      html_nodes(".tbl-cell-vhor .inline-block")%>% html_text(trim=TRUE)%>%gsub(pattern=",",replacement="")%>%as.numeric()%>%
      formattable::accounting(digits=0)
    
    
    
    data.frame(rank,imgs,name, weeks,hours_viewed, views)%>% 
      set_colnames(c("Rank","Poster","Name","Weeks in top 10","Hours Viewed","# of Views "))%>%
      arrange(rank)
  })
  
  data <- eventReactive(input$go,{
url = ifelse(input$type=="Films", 
              paste0("https://www.netflix.com/tudum/top10/",str_to_lower(input$country),"?week=",input$date),
              paste0("https://www.netflix.com/tudum/top10/",str_to_lower(input$country),"/tv?week=",input$date) 
             )# end ifelse

    name = url%>% 
      read_html(.)%>%
      html_nodes(".tbl-cell-name")%>% html_text(trim=TRUE)
    
    weeks =url%>% 
      read_html(.)%>%
      html_nodes(".wk-number")%>% html_text(trim=TRUE)
    
    imgs = url%>%
      read_html()%>%
      html_nodes(".banner-image")%>%html_attr("src")
    
    rank =url%>% 
      read_html(.)%>%
      html_nodes(".tbl-cell-rank")%>% html_text(trim=TRUE)%>%as.numeric()
      
      
    
    data.frame(rank,imgs,name, weeks)%>% 
      set_colnames(c("Rank","Poster",input$type,"Weeks in top 10"))%>%
      arrange(rank)
    
  })# end reactive data
  
output$tbl <- render_gt({
  data()%>%gt()%>%
    tab_header(title=md("***Top 10 by Country***"))%>%
    gt_img_rows(columns="Poster",height=50)%>%
    tab_options(container.height = 400,
                table_body.hlines.color ="red",
                heading.background.color = "red",
                heading.border.bottom.color = "red",
                column_labels.border.bottom.color = "red",
                table.font.color = "white",
                table.background.color="#272c30",
                column_labels.font.size = 15,
                column_labels.font.weight="bold")
    
}) #end output


output$global_table <- render_gt({
  df()%>%gt()%>%
    tab_header(title=md("***Global top 10***"))%>%
    gt_img_rows(columns="Poster",height=50)%>%
    tab_options(container.height = 400,
                table_body.hlines.color ="red",
                heading.background.color = "red",
                heading.border.bottom.color = "red",
                column_labels.border.bottom.color = "red",
                table.font.color = "white",
                table.background.color="#272c30",
                column_labels.font.size = 15,
                column_labels.font.weight="bold")
  
}) #end output
  
}# end server
)# end app


