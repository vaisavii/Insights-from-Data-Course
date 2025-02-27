# Insights from Data
# Project Work 1
# Viivi Väisänen

install.packages("ggplot2")  
library(ggplot2)   

data(mpg)
str(mpg)
print(mpg)

# Task 1

ggplot(mpg, aes(x = class)) +
  geom_bar() +
  labs(title = "Distribution of Car Classes", x = "Car Class", y = "Count") +
  theme_minimal()
# most common car type (class): suv


install.packages("dplyr")
library(dplyr)  


class_proportions <- mpg %>%
  count(class) %>%
  mutate(proportion = n / sum(n))

print(class_proportions)

# class          n     proportion
# <chr>        <int>      <dbl>
# 1 2seater        5     0.0214
# 2 compact       47     0.201 
# 3 midsize       41     0.175 
# 4 minivan       11     0.0470
# 5 pickup        33     0.141 
# 6 subcompact    35     0.150 
# 7 suv           62     0.265 


# Task 2

# scatterplot:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(title = "Engine Displacement vs. Highway Fuel Efficiency",
       x = "Engine Displacement",
       y = "Highway Fuel Efficiency") +
  theme_minimal()

#  negative correlation between engine size and fuel efficiency
#  Scatterplot shows a downward trends as moving to larger values on the
#  x-axis (engine displacement, displ)



# Task 3

# Subset for front-wheel drive (f)
mpg_f <- mpg[mpg$drv == "f", ]

# Subset for rear-wheel drive (r)
mpg_r <- mpg[mpg$drv == "r", ]

# Subset for four-wheel drive (4)
mpg_4 <- mpg[mpg$drv == "4", ]

print(mpg_f)
print(mpg_r)
print(mpg_4)


# mean and standard deviation of highway fuel efficiency (hwy) 
# for each drive type
mean_f_fuel_efficiency <- mean(mpg_f$hwy)
sd_f_fuel_efficiency <- sd(mpg_f$hwy)

mean_r_fuel_efficiency <- mean(mpg_r$hwy)
sd_r_fuel_efficiency <- sd(mpg_r$hwy)

mean_4_fuel_efficiency <- mean(mpg_4$hwy)
sd_4_fuel_efficiency <- sd(mpg_4$hwy)


cat("Front-Wheel Drive (f) - Mean Highway Fuel Efficiency:", mean_f_fuel_efficiency, "\n")
cat("Front-Wheel Drive (f) - Standard Deviation:", sd_f_fuel_efficiency, "\n\n")
# Front-Wheel Drive (f) - Mean Highway Fuel Efficiency: 28.16038 
# Front-Wheel Drive (f) - Standard Deviation: 4.206881

cat("Rear-Wheel Drive (r) - Mean Highway Fuel Efficiency:", mean_r_fuel_efficiency, "\n")
cat("Rear-Wheel Drive (r) - Standard Deviation:", sd_r_fuel_efficiency, "\n\n")
# Rear-Wheel Drive (r) - Mean Highway Fuel Efficiency: 21 
# Rear-Wheel Drive (r) - Standard Deviation: 3.662877


cat("Four-Wheel Drive (4) - Mean Highway Fuel Efficiency:", mean_4_fuel_efficiency, "\n")
cat("Four-Wheel Drive (4) - Standard Deviation:", sd_4_fuel_efficiency, "\n")
# Four-Wheel Drive (4) - Mean Highway Fuel Efficiency: 19.17476
# Four-Wheel Drive (4) - Standard Deviation: 4.07870


# vector fuel_efficiency_means that stores the mean highway fuel efficiency
# for each drive type.
fuel_efficiency_means <- c(fwd = mean_f_fuel_efficiency, rwd = mean_r_fuel_efficiency, 
                           fourwd = mean_4_fuel_efficiency)

# which.max() and which.min() to identify which drive type has the highest 
# and lowest fuel efficiency, respectively.
highest_fuel_efficiency <- names(fuel_efficiency_means)[which.max(fuel_efficiency_means)]
lowest_fuel_efficiency <- names(fuel_efficiency_means)[which.min(fuel_efficiency_means)]


cat("\nDrive type with the highest fuel efficiency:", highest_fuel_efficiency, "\n")
# fwd (f)
cat("Drive type with the lowest fuel efficiency:", lowest_fuel_efficiency, "\n")
# fourwd (4)




# TASK 4

# boxplot:
ggplot(mpg, aes(x = factor(cyl), y= cty)) +
  geom_boxplot() +
  labs(
    title = "Distribution of City Fuel Economy by Number of Cylinders", 
    x = "Number of cylinders",
    y = "City fuel economy"
    ) +
  theme_minimal()




# TASK 5

# Scatterplot of engine displacement (displ) vs. city and highway fuel efficiency
# Dots are the different numbers of cylinders.
ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(color = as.factor(cyl)), alpha = 0.7) +
  labs(title = "City Fuel Efficiency vs. Engine Displacement",
       x = "Engine Displacement (L)",
       y = "City Fuel Efficiency (mpg)",
       color = "Cylinders") +
  theme_minimal()

# same but for highway driving
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = as.factor(cyl)), alpha = 0.7) +
  labs(title = "Highway Fuel Efficiency vs. Engine Displacement",
       x = "Engine Displacement (L)",
       y = "Highway Fuel Efficiency (mpg)",
       color = "Cylinders") +
  theme_minimal()

# Boxplot of city fuel efficiency by class
ggplot(mpg, aes(x = class, y = cty)) +
  geom_boxplot(aes(fill = class)) +
  labs(title = "City Fuel Efficiency by Vehicle Class",
       x = "Vehicle Class",
       y = "City Fuel Efficiency (mpg)") +
  theme_minimal()

# Boxplot of highway fuel efficiency by class
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot(aes(fill = class)) +
  labs(title = "Highway Fuel Efficiency by Vehicle Class",
       x = "Vehicle Class",
       y = "Highway Fuel Efficiency (mpg)") +
  theme_minimal()