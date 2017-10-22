# Load Libraries
devtools::dev_mode('TRUE')
require(plotly)
require(data.table)
require(dplyr)

#load Data
setwd("C:/Users/rahul/Google Drive/RMIT/Semester 2/Data Visualization/Assignment 3")
Cancer = fread('data/2013-cancer.csv')

Cancer$`Year of diagnosis` = as.Date(paste(Cancer$`Year of diagnosis`, 12, 31, sep = "-"))


Bladder.Cancer = Cancer %>%
  filter(`Cancer site/type` == 'C67 Bladder') %>%
  select(Year = `Year of diagnosis`, Cases,Sex) %>%
  group_by(Year,Sex) %>%
  mutate(Count = sum(Cases)) %>%
  select(-Cases) %>%
  unique() %>%
  dcast(Year ~ Sex)

Bladder.Cancer = Bladder.Cancer %>%
  mutate(Total = Males + Females)

updatemenus <- list(
  list(
    active = 0,
    x = -.125,
    type= 'buttons',
    buttons = list(
      list(
        label = "Persons",
        method = "update",
        args = list(list(visible = c(TRUE, "legendonly", "legendonly" )))),
      list(
        label = "Males",
        method = "update",
        args = list(list(visible = c("legendonly", TRUE, "legendonly")))),
      list(
        label = "Females",
        method = "update",
        args = list(list(visible = c("legendonly", "legendonly", TRUE))))
    )
  )
)

p1 = plot_ly(data = Bladder.Cancer) %>%
  add_lines(x=~Year, y=~Total, name = "Persons", line = list(color='#636363'),
            text = ~paste("<b><i>Year</i></b> : ", as.character(Bladder.Cancer$Year,'%Y'), 
                          "<br><i><b>Number of Cases</i></b> : ", Bladder.Cancer$Total)) %>%
  add_lines(x=~Year, y=~Males, name = "Males", visible = "legendonly", line = list(color='#3182bd'),
            text = ~paste("Male Statistics<br>",
                          "<b><i>Year</i></b> : ", as.character(Bladder.Cancer$Year,'%Y'), 
                          "<br><i><b>Number of Cases</i></b> : ", Bladder.Cancer$Males)) %>%
  add_lines(x=~Year, y=~Females, name = "Females", visible = "legendonly", line = list(color='#fc8d59'), 
            text = ~paste("Female Statistics<br>",
                          "<b><i>Year</i></b> : ", as.character(Bladder.Cancer$Year,'%Y'), 
                          "<br><i><b>Number of Cases</i></b> : ", Bladder.Cancer$Females)) %>%
  layout(yaxis = list(zeroline = FALSE, title = 'Number of Cases'),
         xaxis = list(zeroline = FALSE, title = 'Year'),
         title = "Bladder Cancer in Australia, 1982 - 2013",
         updatemenus=updatemenus)
p1

# Read Mortality Data
survival = fread('data/acimcombinedcounts.csv')

survival.1988 = survival %>%
  filter(Year %in% 1984:1988) %>%
  group_by(Cancer_Type,Type) %>%
  summarise(Average = mean(Total)) %>%
  dcast(Cancer_Type ~ Type) %>%
  mutate(Surival_Rate_1988 = (Incidence-Mortality)/Incidence * 100) %>%
  select(-2,-3)

survival.2010 = survival %>%
  filter(Year %in% 2006:2010) %>%
  group_by(Cancer_Type,Type) %>%
  summarise(Average = mean(Total)) %>%
  dcast(Cancer_Type ~ Type) %>%
  mutate(Surival_Rate_2010 = (Incidence-Mortality)/Incidence * 100) %>%
  select(-2,-3)

survival.all = merge(survival.1988,survival.2010, by='Cancer_Type')
survival.all = survival.all %>% 
  mutate(Change = Surival_Rate_2010 - Surival_Rate_1988) %>%
  select(-2,-3)

top.Ten = c('Prostate cancer','Breast cancer','Lung cancer', 
            'Melanoma of the skin','Colon cancer','Rectal cancer',
            'Unknown primary site','Bladder cancer',
            'Stomach cancer','Pancreatic cancer')
survival.all = survival.all %>% filter(Cancer_Type %in% top.Ten) 
survival.all$Cancer_Type = factor(survival.all$Cancer_Type, as.character(rev(top.Ten)))

p2 =  plot_ly(x = ~survival.all$Change, y = ~survival.all$Cancer_Type, name = 'Survival Rate',
              type = 'bar', orientation = 'h',
              text = paste(round(survival.all$Change),'%'), textposition = 'auto',
              marker = list(color = c('rgba(222,45,38,0.8)', 'rgba(39, 175, 195, 0.87)',
                                      'rgba(39, 175, 195, 0.87)', 'rgba(39, 175, 195, 0.87)',
                                      'rgba(39, 175, 195, 0.87)', 'rgba(39, 175, 195, 0.87)',
                                      'rgba(39, 175, 195, 0.87)', 'rgba(39, 175, 195, 0.87)',
                                      'rgba(39, 175, 195, 0.87)', 'rgba(222,45,38,0.8)'),
                            line = list(color = 'rgba(0, 0, 0, .1)', 
                                        width = 1))) %>%
  layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = TRUE, title = ''),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, 
                      showgrid = TRUE, title = 'Percentage Change'),
         margin = list(l = 160)) 

top = Cancer %>%
  select(Cases, `Cancer site/type`) %>%
  group_by(`Cancer site/type`) %>%
  summarise(Count = sum(Cases)) %>%
  arrange(-Count) %>%
  head(10)
top = top$`Cancer site/type`

all.cancer = Cancer %>%
  filter(`Cancer site/type` %in% top) %>%
  select(Cases, `Cancer site/type`,`Year of diagnosis`) %>%
  group_by(`Cancer site/type`,`Year of diagnosis`) %>%
  summarise(Cases = sum(Cases)) %>%
  group_by(`Cancer site/type`) %>%
  summarise(Average = mean(Cases)) %>%
  arrange(-Average)

all.cancer$`Cancer site/type` = factor(all.cancer$`Cancer site/type`, as.character(rev(top)))


p3 = plot_ly(x=~all.cancer$Average, y=~all.cancer$`Cancer site/type`, name = 'Top Diagnosed Cancer',
             type='bar',
             text = paste(round(all.cancer$Average)), textposition = 'auto',
             marker = list(color = 'rgba(39, 175, 195, 0.87)',
                           line = list(color = 'rgba(0, 0, 0, .1)',
                                       width = 1))) %>%
  layout(yaxis = list(showgrid = TRUE, showline = FALSE, showticklabels = FALSE, title = ''),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, 
                      showgrid = TRUE, title = 'Percentage Change')) 

subplot(p2, p3) %>%
  layout(title = '10 most commonly diagnosed Cancers with Survival Rate, Australia, 1984-2013',
         legend = list(x = 0.029, y = 1.038,
                       font = list(size = 10)),
         margin = list(l = 160, r = 20, t = 70, b = 150),
         paper_bgcolor = 'rgb(248, 248, 255)',
         plot_bgcolor = 'rgb(248, 248, 255)') %>%
  add_annotations(xref = 'paper', yref = 'paper',
                  x = -0.14, y = -0.4,
                  
                  align = "left",
                  text = paste('Note<br>
                               1. The rates were age standardised to the 2001 Australian Standard Population and are expressed per 100,000 population.
                               <br>2. Bar lengths indicate the change in survival between the periods 1984-1988 and 2009-2013.
                               <br><b>Source: </b> Australian Cancer Database, 2013
                               <br>Australian Cancer Incidence and Mortality (ACIM) books'),
                  font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                  showarrow = FALSE,titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(x = 0.2 , y = 1.05, text = "5-year relative survival (%)", showarrow = F, xref='paper', yref='paper'),
           list(x = 0.8 , y = 1.05, text = "Most commonly diagnosed Cancer Count", showarrow = F, xref='paper', yref='paper'))
  )

#Plot 3

Bladder.Cancer.Age = Cancer %>%
  filter(`Cancer site/type` == 'C67 Bladder') %>%
  select(Age = `Age group at diagnosis`, Cases) %>%
  group_by(Age) %>%
  summarise(Count = mean(Cases)) %>%
  mutate(Prop = Count/sum(Count) * 100)
  
g1 = ggplot(Bladder.Cancer.Age, aes(Age,Prop)) + 
  geom_bar(stat='identity',fill='rgba(222,45,38,0.6)') +
  ggtitle('Bladder Cancer Percentage Distribution by Age\nAustralia, 1984-2013') +
  xlab('Age Bands') +
  ylab('Percentage') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplotly(g1)

max(g1$data$Count)
