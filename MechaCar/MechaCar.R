mecha_cars <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_cars) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_cars)) #generate summary statistics

suspension <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #read in dataset
total_summary <- suspension %>% summarize(Mean=mean(PSI),Median=median(PSI), Variance=max(PSI) - min(PSI), SD=sd(PSI)) #create summary table with multiple columns
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI), Variance=max(PSI) - min(PSI), SD=sd(PSI)) #create summary table with multiple columns

t.test(suspension$PSI,mu=mean(suspension$PSI)) #compare sample versus population means
lot_summary_1 <- filter(lot_summary, Manufacturing_Lot == "Lot1")
lot_summary_2 <- filter(lot_summary, Manufacturing_Lot == "Lot2")
lot_summary_3 <- filter(lot_summary, Manufacturing_Lot == "Lot3")
t.test(suspension$PSI,mu=mean(lot_summary_1$Mean)) #compare sample versus population means
t.test(suspension$PSI,mu=mean(lot_summary_2$Mean)) #compare sample versus population means
t.test(suspension$PSI,mu=mean(lot_summary_3$Mean)) #compare sample versus population means