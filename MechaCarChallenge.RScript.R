library(dplyr)
Mecha_Car <- read.csv(file="MechaCar_mpg.csv")

model = lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = Mecha_Car)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = Mecha_Car))

SuspensionCoil_data <- read.csv(file = "Suspension_Coil.csv")

total_summary <- SuspensionCoil_data %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance_PSI = var(PSI), standardeviation = sd(PSI))
lot_summary <- SuspensionCoil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_lot = mean(PSI), Median_lot = median(PSI), Variance = var(PSI), standardeviation_lot = sd(PSI), .groups = "keep")


t.test((SuspensionCoil_data$PSI), mu = 1500)
t.test(subset(SuspensionCoil_data,Manufacturing_Lot == "Lot1")$PSI, mu = 1500)
t.test(subset(SuspensionCoil_data,Manufacturing_Lot == "Lot2")$PSI, mu = 1500)
t.test(subset(SuspensionCoil_data,Manufacturing_Lot == "Lot3")$PSI, mu = 1500)
