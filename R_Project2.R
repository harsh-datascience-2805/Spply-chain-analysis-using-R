library(ggcorrplot)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(plotly)
library(reshape2)
library(viridis)
library(hrbrthemes)
library(scales)
library(plyr)
library(pivottabler)

#############################

origin_df = read.csv("C:\\Users\\harsh\\Downloads\\RFinal\\DataCoSupplyChainDataset.csv")
head(origin_df, 3) ; print(paste('shape:',nrow(origin_df),'x',ncol(origin_df)))

#############################

# Find NA

colSums(is.na(origin_df))

############################

# Feature Selection

## correlation matrix between numeric features

num_features_corrplot = function(x){
  num_idx=c()
  for (i in seq(1:ncol(x))){
    if (class(x[,i]) == 'integer'| class(x[,i]) == 'numeric'){
      num_idx = c(num_idx, i)}}
  num_idx = num_idx[-c(8, 24, 28)]  # delete Zipcodes, Product.Status cause they don't show any correlations
  
  options(repr.plot.width = 15, repr.plot.height = 15)
  
  corr = cor(x[num_idx])
  ggcorrplot(corr, type='lower', outline.color='white', 
             lab=T, lab_size=3.5, hc.order=T, colors = c("#6D9EC1", "white", "#E46726"))}

num_features_corrplot(origin_df)

#########################################

# Non-numeric features

num_idx=c()
for (i in seq(1:ncol(origin_df))){
  if (class(origin_df[,i]) == 'integer'| class(origin_df[,i]) == 'numeric'){
    num_idx = c(num_idx, i)}}
num_idx = num_idx[-c(8, 24, 28)]
colnames(origin_df[-num_idx])

#########################################

# EDA

### set related features ###

# char2date
origin_df$order.date..DateOrders. = as.Date(origin_df$order.date..DateOrders., '%m/%d/%Y')
origin_df$shipping.date..DateOrders. = as.Date(origin_df$shipping.date..DateOrders., '%m/%d/%Y')

delv = origin_df[,c(30,1:3,6,7,29,52,53)] 

# order.date : extract date features include days of the week
delv['order.year'] = year(delv$order.date..DateOrders.)
delv['order.month'] = month(delv$order.date..DateOrders.)
delv['order.day'] = day(delv$order.date..DateOrders.)
delv['order.wday'] = wday(delv$order.date..DateOrders., label=TRUE)

# combine by Order.Id
# unique value count : length(unique(delv$Order.Id)) : 65752
n_delv = distinct(delv) ; head(n_delv) ; print(paste('shape:',nrow(n_delv),'x',ncol(n_delv)))
attach(n_delv)

########################################

### Customized plot fuctions ###

### order_counts_per_year ###
order_counts_per_year = function(dataframe){
  df = dataframe %>% 
    group_by(order.month, order.year) %>%
    tally()
  
  options(repr.plot.width = 14, repr.plot.height = 10)
  
  g = ggplot(df, aes(x=order.month, y=n, group=1))+
    geom_point(size=3)+
    geom_line(linetype='dashed')+
    facet_wrap(~order.year)+
    geom_text(aes(label=n), position='dodge', vjust=-1.2, size=5)+
    xlab('\nMonth') + ylab('Total Order Count\n') +
    ylim(1500, 2200)+
    ggtitle('Order Counts per Year(2015-2018)')+
    theme(text = element_text(size=20),
          plot.title = element_text(size=24, face='bold'))
  return(g)}

################################################

### delivery_status_per_year ###
delivery_status_per_year = function(dataframe){
  df = dataframe %>%
    group_by(Delivery.Status, order.month, order.year)%>%
    tally()%>%
    mutate(percent = n/sum(n))
  
  options(repr.plot.width = 16, repr.plot.height = 11)
  
  g = ggplot(df, aes(x=order.month, y=n, fill=Delivery.Status))+
    geom_bar(stat='identity')+
    facet_wrap(~order.year)+
    geom_text(data=subset(df, Delivery.Status != 'Shipping canceled'),
              aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
              position=position_stack(vjust=0.5), colour="white", size = 3.9)+
    ggtitle('Delivery Status per year (2015-2018)')+
    scale_fill_manual(values=c('#16003B','#F73D93', '#413F42', '#7F8487')) +
    scale_x_discrete(breaks=seq(1:12))+
    xlab('\nMonth') +
    ylab('Total Order Count\n') +
    theme_light() +
    theme(text = element_text(size=20),
          plot.title = element_text(size=24, face='bold'),
          legend.justification = c('right', 'bottom'),
          legend.position=c(.98, .28),
          legend.box.background = element_rect(colour='#D0C9C0'))
  
  return(g)}

###################################################

### delivery_status_per_wday ###
delivery_status_per_wday = function(dataframe){
  df = dataframe %>%
    group_by(Delivery.Status, order.wday, order.year)%>%
    tally()%>%
    mutate(percent = n/sum(n))
  
  options(repr.plot.width = 16, repr.plot.height = 11)
  
  g = ggplot(df, aes(x=order.wday, y=n, fill=Delivery.Status))+
    geom_bar(stat='identity')+
    facet_wrap(~order.year)+
    geom_text(data=subset(df, (Delivery.Status != 'Shipping canceled')&
                            (order.year != 2018)),
              aes(label=paste0(sprintf("%1.2f", percent*100),"%")),
              position=position_stack(vjust=0.5), colour="white", size = 4)+
    ggtitle('Delivery Status per Days of Week (2015-2018)')+
    scale_fill_manual(values=c('#9FC088','#E8C07D', '#CC704B', '#614124')) +
    xlab('\nDays of Week')+
    ylab('Total Order Count\n') +
    theme_light() +
    theme(text = element_text(size=20),
          plot.title = element_text(size=24, face='bold'),
          legend.justification = c('right', 'bottom'),
          legend.position=c(.98, .28),
          legend.box.background = element_rect(colour='#D0C9C0'))
  return(g)}

##############################################

### delivery_status_per_type ###
delivery_status_per_ptype = function(dataframe){
  df = dataframe %>%
    group_by(Type, Delivery.Status)%>%
    tally()%>%
    mutate(percent = n/sum(n))  
  
  options(repr.plot.width = 10, repr.plot.height = 8)
  
  g = ggplot(df, aes(x=Type, y=n, fill=Delivery.Status))+
    geom_bar(stat='identity')+
    geom_text(aes(label=paste0(sprintf('%1.1f', percent*100),'%')),
              position=position_stack(vjust=0.5), colour='#203239', size=5)+
    ggtitle('Delivery Status per Payment Type')+
    scale_fill_manual(values=c('#019267','#00C897', '#FFD365', '#FDFFA9'))+
    xlab('\nPayment Type') + ylab('Total Order Count\n')+
    theme_light()+
    theme(text = element_text(size=20),
          plot.title = element_text(face='bold'))
  return(g)}

#################################

order_counts_per_year(n_delv)

################################

delivery_status_per_year(n_delv)

################################

delivery_status_per_wday(n_delv)

################################

delivery_status_per_ptype(n_delv)

################################

# Load the data file
origin_df = read.csv("C:\\Users\\harsh\\Downloads\\RFinal\\DataCoSupplyChainDataset.csv")

# UI function
ui <- fluidPage(
  titlePanel("Data Exploration Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Select a plot to display:"),
      selectInput(inputId = "plot_type", 
                  label = "Select a plot:",
                  choices = c("Correlation Matrix", 
                              "Order Counts per Year", 
                              "Delivery Status per Year", 
                              "Delivery Status per Weekday")
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server function
server <- function(input, output) {
  output$plot <- renderPlot({
    # Correlation Matrix
    if (input$plot_type == "Correlation Matrix") {
      num_idx=c()
      for (i in seq(1:ncol(origin_df))){
        if (class(origin_df[,i]) == 'integer'| class(origin_df[,i]) == 'numeric'){
          num_idx = c(num_idx, i)}}
      num_idx = num_idx[-c(8, 24, 28)]
      corr = cor(origin_df[num_idx])
      ggcorrplot(corr, type='lower', outline.color='white', 
                 lab=T, lab_size=1, hc.order=T, colors = c("#6D9EC1", "white", "#E46726"))
    }
    # Order Counts per Year
    else if (input$plot_type == "Order Counts per Year") {
      n_delv = distinct(delv)
      g = order_counts_per_year(n_delv)
      print(g)
    }
    # Delivery Status per Year
    else if (input$plot_type == "Delivery Status per Year") {
      n_delv = distinct(delv)
      g = delivery_status_per_year(n_delv)
      print(g)
    }
    # Delivery Status per Weekday
    else if (input$plot_type == "Delivery Status per Weekday") {
      n_delv = distinct(delv)
      g = delivery_status_per_wday(n_delv)
      print(g)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

#################################################


######################################################
