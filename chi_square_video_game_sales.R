# Peter Schultz
# Chi-square analysis on video game data sales and confirmed that critic scores influence game success

# load packages
library(tidyverse)
library(readr)
library(magrittr)
library(knitr)
library(kableExtra)
library(sjPlot)
library(DescTools)
library(webshot)
library(pwr)

# load data
vgame_sales <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")


# 2002-2016 decade
# games with ratings by 50 or more critics
critic_set <- vgame_sales %>% filter(Year_of_Release >= 2007 & Year_of_Release <= 2016, 
                                     Critic_Score != "NA", Critic_Count >= 50) %>% droplevels()


# grade amts data frame for Frequency and Percent
# A: 90-100, B: 80-89, C: 70-79, F: below 70
grade_cuts <- c(-Inf, 69, 79, 89, Inf)
grade_labs <- c("F", "C", "B", "A")
critic_set %<>% mutate(GR_cat = cut(Critic_Score, br = grade_cuts, labels = grade_labs)) %>% 
  mutate(GR_cat = fct_relevel(GR_cat, rev))

df_grade_amts <- critic_set %>% 
  group_by(GR_cat) %>%
  summarize(Frequency = n(),
            Percent = paste0(round(n()/dim(critic_set)[1]*100, digits = 1), "%"))
df_grade_amts


# bad, good, great sales divided into quartiles
# bad sale: <25% overall , good sale: 25-75% overall, great sale: >75% overall
bgg_sales <- quantile(critic_set$Global_Sales)
bgg_cuts <- c(-Inf, bgg_sales[2], bgg_sales[4], Inf)
bgg_labs <- c("Bad Sales", "Good Sales", "Great Sales")
critic_set %<>% mutate(BGG_cat = cut(Global_Sales, br = bgg_cuts, labels = bgg_labs))

df_g_sales_amts <- critic_set %>% 
  group_by(BGG_cat) %>%
  summarize(Frequency = n(),
            Percent = paste0(round(n()/dim(critic_set)[1]*100, digits = 1), "%"))
df_g_sales_amts


# Frequency Table
colnames(df_grade_amts)[1] <- "Category"
colnames(df_g_sales_amts)[1] <- "Category"
df_cat <- rbind(df_grade_amts, df_g_sales_amts)
tname <- "Table 1: Univariate Distribution of Grade and Sales"
titlehead <- c(tname = 3)
names(titlehead) <- tname

unicat <- df_cat %>% kable(booktabs = T, align = "lcc") %>% 
  kable_styling(full_width = FALSE) %>% 
  pack_rows("Grade", 1, 4) %>% pack_rows("Sales", 5, 7) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(df_cat), extra_css = "border-bottom: solid;") %>%
  save_kable("freq_perc.pdf")
unicat


# Multibar Chart
critic_set %>% 
  group_by(GR_cat, BGG_cat) %>% summarize(freq = n()) %>% ungroup() %>% 
  group_by(GR_cat) %>% mutate(pct_within_educ = freq / sum(freq) * 100) %>% 
  # make the plot 
  ggplot(aes(x = GR_cat, fill=BGG_cat, y=pct_within_educ)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Critic Grade",
       y = "Percent",
       fill = "Sales",
       title = "Sales By Critic Grade") +
  ## optional customizations
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("Red", "Blue", "Green")) +
  ## add percentage labels at top of bars
  geom_text(aes(label=paste0(round(pct_within_educ),"%")), 
            vjust=1.2, color="black", position = position_dodge(0.9), size=5)
ggsave("multibar.png")


# Chi-Square
critic_table <- table(critic_set$GR_cat, critic_set$BGG_cat)
c2 <- chisq.test(critic_table)
c2


# Effect Size
effsize <- CramerV(critic_table)
effsize


# Two-way Table
tab_xtab(var.row = critic_set$GR_cat, ## variable that makes up the rows
         var.col = critic_set$BGG_cat,  ### variable that makes up the columns
         ### specify descriptive overall table title
         title = "Table 2: Chi-Square Test - Sales by Critic Grade",
         ## specify variable labels in order of row then column (as a vector of strings)
         var.labels = c("Grade", "Sales"),
         show.cell.prc = TRUE, ## show percentages in the cells
         show.row.prc = TRUE,
         show.summary = TRUE, ## to get chi-square
         statistics = "cramer",
         file = "grade_sales.html"
)
webshot("grade_sales.html", "grade_sales.png")


# Power test
pwr.chisq.test(w = effsize, N = NULL, df = c2$parameter, sig.level = 0.5, power = 0.8)

