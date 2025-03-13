library(dplyr)
library(ggplot2)
library(tidyr)

# Load all FAERS drug data for 2019 (each quarter)
q1 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/DRUG19Q1.txt", sep = "$", stringsAsFactors = FALSE)
q2 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/DRUG19Q2.txt", sep = "$", stringsAsFactors = FALSE)
q3 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/DRUG19Q3.txt", sep = "$", stringsAsFactors = FALSE)
q4 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/DRUG19Q4.txt", sep = "$", stringsAsFactors = FALSE)

# Merge all drug data
faers_2019 <- bind_rows(q1, q2, q3, q4)
#write.csv(faers_2019, "FAERS_2019_Merged.csv", row.names = FALSE)

# Load all FAERS reaction data for 2019 (each quarter)
reac_q1 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/REAC19Q1.txt", sep = "$", stringsAsFactors = FALSE)
reac_q2 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/REAC19Q2.txt", sep = "$", stringsAsFactors = FALSE)
reac_q3 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/REAC19Q3.txt", sep = "$", stringsAsFactors = FALSE)
reac_q4 <- read.csv("C:/Users/User/OneDrive/Documents/Digital Intelligence/Drug/REAC19Q4.txt", sep = "$", stringsAsFactors = FALSE)

# Merge all reaction data
reac_2019 <- bind_rows(reac_q1, reac_q2, reac_q3, reac_q4)
#write.csv(reac_2019, "REAC_2019_Merged.csv", row.names = FALSE)

# Filter Tramal 
tramal_reports <- faers_2019 %>%
  filter(toupper(drugname) == "TRAMAL") %>%  
  select(primaryid)  

#######################################################################################################################

#Get Adverse Reactions for Tramal
tramal_reactions <- reac_2019 %>%
  filter(primaryid %in% tramal_reports$primaryid) %>%
  group_by(pt) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)  # Top 10 most common adverse effects

ggplot(tramal_reactions, aes(x = reorder(pt, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Top 10 Adverse Effects of Tramal (FAERS 2019)",
       x = "Adverse Effect",
       y = "Count") +
  theme_minimal()

########################################################################################################################

# Filter Tramal cases
tramal_reports <- faers_2019 %>%
  filter(toupper(drugname) == "TRAMAL") %>%
  select(primaryid)

# Filter Lyrica cases
lyrica_reports <- faers_2019 %>%
  filter(toupper(drugname) == "LYRICA") %>%
  select(primaryid)

# Tramal adverse reactions
tramal_reactions <- reac_2019 %>%
  filter(primaryid %in% tramal_reports$primaryid) %>%
  group_by(pt) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)  

# Lyrica adverse reactions
lyrica_reactions <- reac_2019 %>%
  filter(primaryid %in% lyrica_reports$primaryid) %>%
  group_by(pt) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)  

# Rename columns for clarity
tramal_reactions <- tramal_reactions %>% rename(tramal_count = count)
lyrica_reactions <- lyrica_reactions %>% rename(lyrica_count = count)

# Merge on adverse reaction term
comparison <- full_join(tramal_reactions, lyrica_reactions, by = "pt") %>%
  replace_na(list(tramal_count = 0, lyrica_count = 0))  # Fill missing values with 0

# Convert data to long format for grouped bar plot
comparison_long <- comparison %>%
  pivot_longer(cols = c(tramal_count, lyrica_count), 
               names_to = "drug", 
               values_to = "count")

# Plot comparison
ggplot(comparison_long, aes(x = reorder(pt, count), y = count, fill = drug)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Comparison of Adverse Effects: Tramal vs. Lyrica",
       x = "Adverse Effect",
       y = "Count",
       fill = "Drug") +
  theme_minimal()
