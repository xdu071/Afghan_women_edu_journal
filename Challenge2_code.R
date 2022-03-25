# Challenge 2: BBC Pitch - Visualizing inequalities females face in education (Afghanistan)
# Group 5: Ailsa, Ana, Barbora, David, Helene, Louise
# Data source: https://www.education-inequalities.org/countries/afghanistan#dimension=%7B%22id%22%3A%22sex%22%2C%22filters%22%3A%5B%5D%7D&year=%222015%22

# Libraries ----

library(tidyverse)
library(ggplot2)  # for Aisla or if you can't use tidyverse
library(dplyr)
library(tidyr)
library(gridExtra) # for making basic panels
library(grid)  # for making for cusotmizable panels


# Load and check data ----

data <- read.csv("raw_dataset/WIDE_2021-01-28_v1.csv")  # loads data on education indicators in Afghanistan (2015)
str(data)  # structure of data 

# David's work -> create custom color palette and theme ----
# Creating color palette
journal_palette <- c("")

# David's work = creating main theme ----
# Creating custom themes
david_style <- function(){
  font <- "Helvetica"
  theme(plot.title = element_text(family = font, size = 14, face = "bold", color = "#222222", hjust = 0.5), 
        plot.subtitle = element_text(family = font, size = 12, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.position = "none",
        legend.text.align = 0, 
        legend.title = element_blank(), 
        legend.key = element_blank(), 
        legend.text = element_text(family = font, size = 9, color = "#222222"),
        axis.text = element_text(family = font, size = 9, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.title = element_text(family = font, size = 12, face = "bold", color = "#222222"), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(color = "#cbcbcb"), 
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(size = 12, hjust = 0))
}


# Louise's work = Data wrangling before making the graphs ----

AFG_data <- data %>% 
  filter(country == "Afghanistan")  %>%  # selecting only AFG data
  select(-iso_code,  #  column name might also be ï..iso_code depending on laptop (change the code accordingly)
         -region_group, -income_group, -country, -grade, -level,  
         -slevel4_no, -slevel4_m, -rlevel4_no, -rlevel4_m, -mlevel4_no, 
         -mlevel4_m, -slevel3_no, -slevel3_m, -rlevel3_no, -rlevel3_m, 
         -mlevel3_no, -mlevel3_m, -slevel2_no, -slevel2_m, -rlevel2_no, 
         -rlevel2_m, -mlevel2_no, -mlevel2_m, -slevel1_no, -slevel1_m, 
         -rlevel1_no, -rlevel1_m, -mlevel1_no, -mlevel1_m, -eduout_lowsec_no,
         -eduout_prim_no, -eduout_upsec_no) %>%  # taking away columns that don't matter anymore OR don't have any data
  filter(!grepl("MICS", survey))  # only selecting the recent survey

# Making new object for sex comparison graph ----

AFG_data_sex <- AFG_data %>% 
  filter(category == "Sex")  %>%  # selecting sex differences 
  select(-Location, -Wealth, -Region, -Ethnicity, -Religion, -Language, -category) %>% 
  pivot_longer(data =., cols = 4:46, names_to = "indicator", values_to = "percentage") %>% 
  filter(!grepl("_no", indicator)) %>% 
  mutate(age_group = case_when(  # create age column with info from indicators 
    grepl("1822", indicator) ~ "18-22", 
    grepl("2529", indicator) ~ "25-29", 
    grepl("3034", indicator) ~ "30-34", 
    grepl("1524", indicator) ~ "15-24", 
    grepl("2029", indicator) ~ "20-29", 
    grepl("v2", indicator) ~ "3-4 years after graduation age", 
    grepl("edu0_prim", indicator) ~ "9-12", 
    grepl("2024", indicator) ~ "20-24", 
    grepl("eduout_lowsec", indicator) ~ "adolescent", 
    grepl("eduout_prim", indicator) ~ "children", 
    grepl("eduout_upsec", indicator) ~ "youth", 
    grepl("overage2plus", indicator) ~ "childen", 
    grepl("preschool_1ybefore", indicator) ~ "children", 
    grepl("preschool_3", indicator) ~ "3-4", 
    grepl("trans_lowsec", indicator) ~ "youth",  
    grepl("trans_prim", indicator) ~ "adolescent")) %>% 
 mutate(indicator_name = case_when(  # new indicator column to be more informative 
    grepl("attend_higher_1822", indicator) ~ "Attended higher education", 
    grepl("comp_higher_2yrs", indicator) ~ "Completed 2 years of higher education",
    grepl("comp_higher_4yrs", indicator) ~ "Completed 4 years of higher education",
    grepl("comp_lowsec", indicator) ~ "Completed lower secondary school",
    grepl("comp_prim", indicator) ~ "Completed primary school", 
    grepl("comp_upsec", indicator) ~ "Completed upper secondary school",
    grepl("edu0_prim", indicator) ~ "Never attended school",
    grepl("edu2", indicator) ~ "Completed 2 years of education",
    grepl("edu4", indicator) ~ "Completed 4 years of education", 
    grepl("eduout_lowsec", indicator) ~ "Out of school adolescents",
    grepl("eduout_prim", indicator) ~ "Out of school children",
    grepl("eduout_upsec", indicator) ~ "Out of school youth",
    grepl("eduyears", indicator) ~ "Mean years of education", 
    grepl("literacy", indicator) ~ "Youth literacy rate",
    grepl("overage2plus", indicator) ~ "Overage primary students",
    grepl("preschool_1ybefore", indicator) ~ "Primary school attendance 1 year early",
    grepl("preschool_3", indicator) ~ "Primary school attendance",
    grepl("trans_lowsec", indicator) ~ "Transition rate to upper secondary school", 
    grepl("trans_prim", indicator) ~ "Transition rate to lower secondary school")) %>% 
  rename(sex = Sex) %>% 
  filter(!is.na(percentage), !percentage < 0.04, !indicator == "eduyears_2024_m") %>%  # eduyears is not a % so won't work in our graph
  mutate(percentage = percentage*100)  # changing fraction values into percentages

str(AFG_data_sex)  # checking the manipulation worked well

# Data manipulation for female x wealth/ethnicity graphs ----

# Wealth dataframe 

AFG_data_wealth <- AFG_data %>% 
  filter(grepl("Sex", category), 
         !grepl("Location", category),
         !grepl("Region", category), 
         !grepl("Religion", category), 
         !grepl("Ethnicity", category)) %>% 
  select(-Location, -Region, -Ethnicity, -Religion, -Language) %>% 
  pivot_longer(data =., cols = 6:48, names_to = "indicator", values_to = "percentage") %>% 
  filter(!grepl("_no", indicator), !grepl("Male", Sex), indicator == "literacy_1524_m") %>% 
  rename(sex = Sex, wealth = Wealth) %>% 
  mutate(percentage = percentage*100)

# Ethnicity dataframe

AFG_data_ethnicity <- AFG_data %>% 
  filter(grepl("Sex", category), 
         !grepl("Location", category),
         !grepl("Region", category), 
         !grepl("Religion", category), 
         !grepl("Wealth", category)) %>% 
  select(-Location, -Region, -Wealth, -Religion, -Language) %>% 
  pivot_longer(data =., cols = 6:48, names_to = "indicator", values_to = "percentage") %>% 
  filter(!grepl("_no", indicator), !grepl("Male", Sex), indicator == "literacy_1524_m") %>% 
  rename(sex = Sex, ethnicity = Ethnicity) %>% 
  mutate(percentage = percentage*100)

# Ana's work = creating the wealth vs. female barplot on youth literacy rates ----
# Creating colour palette for plot
wealth_palette <- c("#E63946", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC")  # assigns colours to wealth_palette
names(wealth_palette) <- levels(AFG_data_wealth$wealth)  # assigns colors to wealth factor levels in AFG_data_wealth data frame

# Plotting wealth x female literacy
(AFG_wealth_bar <- AFG_data_wealth %>% 
    ggplot(aes(x = reorder(wealth, desc(percentage)), y = percentage)) + 
    geom_bar(stat = "identity", aes(fill = wealth), color = "black") + 
    labs(x = "\nWealth", y = "Youth Literacy Rate (%)\n", title = "Influence of Wealth on Female Youth Literacy Rates") +
    scale_fill_manual(values = wealth_palette) + 
    scale_x_discrete(labels = c("Quintile 5", "Overall", "Quintile 4", "Quintile 3", "Quintile 1", "Quintile 2")) +
    scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
    david_style())


#Helene's plot = creating the male vs. female education indicators plot ----

(main_plot <- ggplot(data=AFG_data_sex, aes(x=indicator_name, y=percentage, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=percentage), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() + 
  coord_flip() + 
  ggtitle("Afghanistan educational sex differences") +
  xlab("") + ylab("Percentage") +
  david_style())

# Ailsa's Plot= ethnicity vs. female youth literacy rates ----

# Adding 'Total Female' to Ethnicity dataset
AFG_data_ethnicity$ethnicity <- as.character(AFG_data_ethnicity$ethnicity)
AFG_data_ethnicity[1,5] <- "Overall"

# Creating colour palette for plot 
ethnicity_palette <- c("#E63946", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC", "#A8DADC")  # assigns colours to ethnicity palette 
names(ethnicity_palette) <- levels(AFG_data_ethnicity$ethnicity)  # assigns colors to ethnicity factor levels in AFG_data_ethnicity data frame

# Plotting ethnicity x female literacy
(AFG_ethnicity_bar<-AFG_data_ethnicity %>% 
    ggplot(aes(x=reorder(ethnicity, desc(percentage)), y=percentage, fill=category)) +
    geom_bar(stat="identity", colour = "black") +
    labs(x="\nEthnicity", 
         y="Youth Literacy Rate (%)\n", 
         title="Ethnicity as a Factor on Female Youth Literacy Rates") +
    scale_fill_manual(values = ethnicity_palette) +
    david_style())

# Arranging all plots into panels ----

# Panel version 1
(AFG_panel <- grid.arrange(
  main_plot + ggtitle("(a) Afghanistan educational sex differences"),
  arrangeGrob(
    AFG_wealth_bar + ggtitle("(b) Influence of Wealth on Female Youth Literacy Rates"),
    AFG_ethnicity_bar + ggtitle("(c) Ethnicity as a Factor on Female Youth Literacy Rates")
    ),
  ncol = 2
  ))

# Panel version 2 
# Move to a new graph page
grid.newpage()

# Create layout of graph, nrow = 2, ncol = 2
pushViewport(viewport(layout = grid.layout(2, 2)))

# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

# Arrange  plots
print(main_plot, vp=define_region(1, 1:2))
print(AFG_wealth_bar, vp = define_region(2, 1))
print(AFG_ethnicity_bar, vp = define_region(2, 2))

ggsave(AFG_ethnicity_bar, file = "img/female_ethnicity_youth_lit.png", height = 5, width = 8)
