#Linear Regression to predict MPG

#importing libraries
library(dplyr)
#importing relevant csv into a df
Mecha_mpg_df <- read.csv("resources/MechaCar_mpg.csv")
#generating a linear model based on all the columns and mpg
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=Mecha_mpg_df)
#generating a statistical summary of the model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=Mecha_mpg_df))


#Summary Statistics on Suspension Coils

#importing relevant csv
suspension_coil_table <- read.csv("resources/Suspension_Coil.csv")
#making a summary based on PSI for all lots
total_summary <- suspension_coil_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
#making a summary based on PSI for each lot
lot_summary <- suspension_coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')


#T-Tests on Suspension Coils

#t test for the suspension coils for all lots
t.test(suspension_coil_table$PSI, mu=1500)
#t test for the suspension coils for each lot
t.test(subset(suspension_coil_table, suspension_coil_table$Manufacturing_Lot == "Lot1")$PSI, mu=1500)

t.test(subset(suspension_coil_table, suspension_coil_table$Manufacturing_Lot == "Lot2")$PSI, mu=1500)

t.test(subset(suspension_coil_table, suspension_coil_table$Manufacturing_Lot == "Lot3")$PSI, mu=1500)
