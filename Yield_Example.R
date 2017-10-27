requiredPackages <- c('tidyverse','broom')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

################1. Import, inspect, and mutate US_Data###############

us_data <- read_csv("US_Yield_Data.csv", na = "")
head(us_data)

summary(us_data)

us_data <- us_data %>%
  mutate(Yield_kg_ha = Yield_bu_acre*62.765,
         Period = ifelse(Year <= 1936, "OPV",
                         ifelse(Year <= 1955, "Double Cross Hybrid",
                                ifelse(Year <= 1995, "Single Cross Hybrid",
                                       "Biotech"))))
head(us_data)

########2. Create a model function ############
le_lin_fit <- function(data) {
  the_fit_ha <- lm(Yield_kg_ha ~ Year, data)
  the_fit_bu <- lm(Yield_bu_acre ~ Year,data)
  return(as.tibble(cbind(slope_ha = coef(the_fit_ha)[2],slope_bu = coef(the_fit_bu)[2])))
}


#######3a. Regression analysis across periods #########
us_data_reg <- us_data %>%
  group_by(Period) %>%
  do(le_lin_fit(.)) %>%
  mutate(xax = ifelse(Period == "OPV",1895,
                      ifelse(Period == "Double Cross Hybrid",1930,
                             ifelse(Period == "Single Cross Hybrid",1950,1985))),
         yax = ifelse(Period == "OPV",2750,
                      ifelse(Period == "Double Cross Hybrid",3250,
                             ifelse(Period == "Single Cross Hybrid",6500,10500))))
head(us_data_reg)

########3b. Alternative method 1 #######
us_data_reg_alt <- us_data %>%
  group_by(Period) %>%
  do(tidy(lm(Yield_bu_acre ~ Year, .)))
head(us_data_reg_alt)

#######3c. Alternative method 2 ###########
us_data_reg_alt2 <- us_data %>%
  group_by(Period) %>%
  do(fit = (lm(Yield_bu_acre ~ Year, .)))
head(us_data_reg_alt2)

us_data_reg_alt2 %>%
  tidy(fit)


#####4. Setting the priod names. #######
period_Name <- c("OPV\n(1866-1936)\n","Double Cross Hybrid\n(1937-1955)\n",
                 "Single Cross Hybrid\n(1956-1995)\n",
                 "Biotech\n(1996 - Current)\n")


#######5. Plotting the data ######
us_yield <- us_data %>%
  mutate(Period = factor(Period,levels = c("OPV","Double Cross Hybrid",
                                           "Single Cross Hybrid","Biotech"))) %>%
  ggplot(aes(x = Year,y = Yield_kg_ha, group = Period)) +
  geom_point(aes(color = Period, shape = Period), size = 2.5) +
  theme_bw() +
  geom_smooth(aes(color = Period),method = 'lm', se = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(from = 1865, to = 2016,by = 25)) +
  labs(title = "Maize Yield over the last 150 years",
       y = "Average Maize Yields (kg/ha)",
       caption = "Data from USDA-NASS") +
  scale_y_continuous(breaks = seq(from = 1000, to = 11000, by = 1000),
                     sec.axis = sec_axis( ~./62.765, 
                                          name = "Average Maize Yields (Bu/Acre)",
                                          breaks = seq(from = 15, to = 175, by = 15))) +
  geom_text(data = us_data_reg,aes(xax, yax, color = Period,
                                   label = paste("b =" ,round(slope_ha,2),
                                                 "kg/ha\n        ",round(slope_bu,2),
                                                 "bu/acre",sep = "")),
            size = 4.5,fontface = "bold",show.legend = FALSE) +
  scale_color_manual(values = c("DarkGreen","Purple","Red","Blue"),
                     labels = period_Name) +
  scale_shape_manual(values = c(15,16,17,18),
                     labels = period_Name) +
  theme(aspect.ratio = .5)
us_yield



