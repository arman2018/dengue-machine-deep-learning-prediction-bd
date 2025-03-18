#Dengue ML & DL
##My directory
setwd("D:/Research/Dengue_DL & ML")

#Load required library
library(readxl)
library(dplyr)
library(tidyr)
library(car)


#Upload the data
data<-read_excel("data_DENV.xlsx",sheet="data")

##Basic statistics

# Convert the data from wide to long format
df_long <- data %>%
  pivot_longer(
    cols = c(y, x1, x2, x3, x4, x5, x6, x7),
    names_to = "variable",
    values_to = "value"
  )

# Calculate descriptive statistics for each variable by year
desc_stats <- df_long %>%
  group_by(Year, variable) %>%
  summarise(
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    .groups = "drop"
  )

# Display the resulting table
print(desc_stats)
View(desc_stats)



# Calculate overall descriptive statistics for each variable
desc_stats <- df_long %>%
  group_by(variable) %>%
  summarise(
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE)
  )

# Display the resulting table
print(desc_stats)
View(desc_stats)

##Monthly distribution precipitation in BD
data$Months <- month(data$Months, label = TRUE)  # Label = TRUE gives month names

# Plot the monthly boxplot
ggplot(data, aes(x = Months, y = x5)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Monthly Distribution of Precipitation (2000-2022)",
       x = "Months", y = "Precipitation (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Multicollinearity check
model_linear<-lm(y~x1+x2+x3+x4+x5+x6+x7,data=data)

VIF<-vif(model_linear)
print(VIF)

