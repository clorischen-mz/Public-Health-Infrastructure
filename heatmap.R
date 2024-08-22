install.packages("tidyverse")
install.packages("tidycensus")
install.packages("usmap")
install.packages("ggplot2")


library(usmap)
library(ggplot2)

#states <- c("Alabama", "Alaska", "Arkansas", "Arizona", "California", "Colorado", "Conneticut", "Delaware", "Washington DC", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Massachussets", "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "Mississipi", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Washington", "West Viriginia", "Wisonsin", "Wyoming")
state <- c("AL", "AK", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "WA", "WV", "WI", "WY","VT")
percentage <- c(20.9, 3.45, 25.33, 53.33, 37.93, 31.25, 12.50, 33.33, 41.79, 21.38, 0.00, 84.09, 25.49, 30.43, 18.18, 18.10, 35.00, 53.13, 12.50, 33.33, 12.50, 30.12, 16.09, 19.13, 17.07, 30.36, 37.63, 17.65, 20.00, 33.33, 24.24, 37.10, 25.00, 35.85, 34.09, 16.88, 44.44, 14.93, 0.00, 67.39, 15.15, 17.89, 15.35, 37.93, 43.28, 43.59, 29.09, 29.17, 26.09,0.00)
data <- data.frame(state, percentage)


usmap <- plot_usmap(regions = "state", data = data, values = "percentage", color = "red",labels = TRUE) +
  scale_fill_continuous(low = "white", high = "red", name = "Response") +
  labs(title = "U.S. States",
       subtitle = "These are the Response Rates to NACCHO and NALSYS Surveys by the LHDs in Each State Based on Analytic Sample") + 
  theme(panel.background=element_blank())

usmap$layers[[2]]$aes_params$size <- 2
print(usmap)

#plot_usmap(data = data, values = "percentages", color = "red") + 
 # scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  #theme(legend.position = "right")

#plot_usmap(data = data, values = "percentages", color = "red") + 
 # scale_fill_continuous(low = "white", high = "red", name = "Response", label = scales::comma) 

