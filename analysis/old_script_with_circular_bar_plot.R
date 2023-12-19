##### FLINDERS TRAINING & DEVELOPMENT SESSIONS 2021 #####
##### Jennifer Beaudry ####

##### PREPROCESSING THE DATA #####


##### LIBRARY #####

library(tidyverse)
library(here)
library(tools)
library(readxl)
library(formattable)

# useful blog for formattable
# https://www.r-bloggers.com/2018/11/make-beautiful-tables-with-the-formattable-package/


####### FUNCTIONS ############
# from Mathew Ling's Misinformation Github repo
# https://github.com/Lingtax/misinformation/blob/master/R/read_qualtrics.R

# Mathew Ling's 'meta_rename' function

meta_rename <-  function(df, metadata, old, new) {

  keys   <- metadata[[deparse(substitute(old))]]
  values <- metadata[[deparse(substitute(new))]]
  rename_at(df, vars(all_of(keys)), ~ values)
}

# Matt Williams's missing data function; counts how many NAs there are

miss_fun = function(x){
  sum(is.na(x))
}

##### LOAD DATA #####

df <- read_xlsx(
  here::here("data",
             "2021 Planning Timetable.xlsx"),
  sheet = '2021 Program',
  skip = 7,
  n_max = 69, #last session is Human Ethics on Dec 9th
  .name_repair = "unique",
  trim_ws = TRUE)

#### SELECT RELEVANT COLUMNS & CONVERT FACTORS ####

df_all <- df %>%
  select (c(Theme, `% attend`, Session, Date, `Team / Provider`, `Presenter One`, `Presenter Two`)) %>%
  rename (attended = `% attend`)

df_all$Theme <- as.factor(df_all$Theme)
df_all$Session <- as.factor(df_all$Session)

#### FILTER OUT SESSIONS THAT WE HAVEN'T RUN YET ####
df_run <- df_all %>%
  filter (attended != 'NA')

#### BREAKDOWN BY THEME ###

# decide whether I want the full data set (df_all) or just the sessions
  # we've run to date (df_run)
df <- df_run

# how many sessions per theme
theme <- df %>%
  group_by(Theme) %>%
  summarise(`Number of Sessions` = n()) %>%
  arrange (desc(`Number of Sessions`))

# make it look good
  formattable(theme,
              align = c("l", "c"))

# how many attendees per theme
attendees <- df %>%
  group_by(Theme) %>%
  summarise(`Number of Attendees` = sum(attended)) %>%
  arrange (desc(`Number of Attendees`))

# make it look good
formattable(attendees,
            align = c("l", "c"))

#combine sessions & attendees per theme
both <- df %>%
  group_by(Theme) %>%
  summarise(`Number of Sessions` = n(),
            `Number of Attendees` = sum(attended)) %>%
  arrange (desc(`Number of Attendees`))

# make it look good
formattable(both,
            align = c("l", "c", "c"))

#### VISUALISE THE DATA ####

# trying to make a circular bar plot
# https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html

#### attendees per session by theme ####

# add id column and arrange by Theme & number attended (so it's ordered)
df <- df %>%
  arrange(Theme, attended) %>%
  mutate(id = 1:n())


# add info about labels and positions of labels
label_data <- df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I subtract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


# create the plot
p1 <- ggplot(df, aes(x=as.factor(id), y=attended, fill=Theme)) +
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=attended+10, label=attended, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.0, angle= label_data$angle, inherit.aes = FALSE )

p1

#### attendees per theme ####

# add id column and arrange by Theme & number attended (so it's ordered)
  # and group by theme!
df <- df %>%
  group_by(Theme) %>%
  summarise(attended = sum(attended)) %>%
  arrange(attended) %>%
  mutate(id = 1:n())

# add info about labels and positions of labels
label_data <- df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I subtract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


# create the plot
p2 <- ggplot(df, aes(x=as.factor(id), y=attended, fill=Theme)) +
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,650) + #update limits for appropriate data
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=attended+5, label=attended, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.0, angle= label_data$angle, inherit.aes = FALSE )

p2




