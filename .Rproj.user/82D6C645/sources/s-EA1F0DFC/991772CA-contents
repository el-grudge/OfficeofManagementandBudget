library(tidyverse)
library(readxl)
data <- read.csv("budgets.csv")
df <- data.frame(data)

df_2021dev <- select(df, category, X2020, X2021)
df_2021dev$dev <- (df_2021dev$X2021-df_2021dev$X2020) / df_2021dev$X2020
df_2021dev$devz <- round((df_2021dev$dev - mean(df_2021dev$dev)) / sd(df_2021dev$dev),2)
df_2021dev$thresh <- ifelse(df_2021dev$devz > 0, "above", "below")
df_2021dev <- df_2021dev[order(df_2021dev$devz),]
df_2021dev$category <- factor(df_2021dev$category, levels = df_2021dev$category)

# Diverging bar chart
ggplot(df_2021dev, aes(x=category, y=devz, label=devz)) + 
  geom_bar(stat='identity', aes(fill=df_2021dev$thresh), width=.9)  +
  scale_fill_manual(name="Budget Deviation", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Budget Deviation between years 2020 & 2021", 
       title= "Diverging Bar Chart") + 
  coord_flip()

# Group bar chart
df_grp <- df
df_grp <- df_grp %>% select(-category, -X2019, -X2020) %>% gather(key = year, value = amount, X2021,X2022,X2023,X2024,X2025,X2026,X2027,X2028,X2029,X2030)

for(i in 1:length(unique(df_grp$year))){
  df_grp$year[df_grp$year == unique(df_grp$year)[i]] = substring(unique(df_grp$year)[i], 2)
}

df_grp %>% 
  ggplot(aes(year, amount)) +
  geom_bar(aes(fill = programs, color = programs), stat = "identity", position = "dodge", width = 0.5) + 
  theme(legend.position="top") + 
  labs(subtitle="Budget Spending between from 2021 to 2030", 
       title= "Group Bar Chart")

