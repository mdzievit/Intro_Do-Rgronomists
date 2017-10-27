requiredPackages <- c('tidyverse','broom')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

######1. Read data and inspect #######
state_data <- read_csv("US_State_Yield_Data.csv")
state_data <- state_data %>%
  mutate(Period = ifelse(Year <= 1936, "OPV",
                        ifelse(Year <= 1955, "Double Cross Hybrid",
                               ifelse(Year <= 1995, "Single Cross Hybrid","Biotech"))),
        Period = factor(Period,levels = c("OPV","Single Cross Hybrid",
                                          "Double Cross Hybrid","Biotech")))
#####2. Set period names ######
period_Name <- c("Single Cross Hybrid\n(1956-1995)\n",
                 "Biotech\n(1996-Current)\n")

####3. Create linear models and store as function ##########
le_lin_fit <- function(data) {
  
  the_fit_ha <-tibble(kg_ha = coef(lm(Yield_kg_ha ~ Year,data = data))[2])
  the_fit_bu <- tibble(bu_acre = coef(lm(Yield_bu_acre ~ Year,data = data))[2])
  return(cbind(the_fit_ha,the_fit_bu))
}


######4.  "Do" linear models across groups ############
slope <- state_data %>%
  group_by(State) %>%
  do(le_lin_fit(.))


###5. Plot the data #######
state_yield_all <- state_data %>%
  ggplot(aes(x = Year, y = Yield_kg_ha)) +
  facet_wrap(~State) +
  geom_point(aes(color = Period, shape = Period), size = 3) +
  geom_smooth(method = 'lm', se = FALSE, color = "Black") +
  scale_x_continuous(breaks = seq(from = 1965, to = 2016,by = 10)) +
  scale_y_continuous(breaks = seq(from = 4000, to = 16000, by = 2000),
                     name = "Average Maize Yields (kg/ha)",
                     sec.axis = sec_axis( ~./62.765,
                                          name = "Average Maize Yields (Bu/Acre)",
                                          breaks = seq(from = 75, to = 200, by = 25))) +
  geom_text(data = slope, aes(x = 1992, y = 11500,
                              label = paste("b = ",round(kg_ha,3)," kg/ha \n              ",
                                            round(bu_acre,3)," bu/acre",sep = "")),
            fontface = "bold", size = 2.5) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  labs(title = "Yield in 6 other states since 1982",
       caption = "Data from USDA-NASS") +
  theme(aspect.ratio = .5) +
  scale_color_manual(values = c("Red","Blue"),
                     label = period_Name) +
  scale_shape_manual(values = c(17,18),
                     label = period_Name)

state_yield_all
