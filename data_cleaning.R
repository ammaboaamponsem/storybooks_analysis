cat("\014") # clears console
rm(list = ls()) #clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = True) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(pacman)
library(dplyr)
p_load(tidyverse)

library(readr) # Install the readr package first to read a file

# Read data from a CSV file
data <- read_csv("books.csv")
data

# Install and load the janitor package first to use the clean_name function
library(janitor)

# Clean the column names in your data frame
books.df <- clean_names(data)
books.df

# Install and load the lubridate package first to use the mdy function
library(lubridate)

# Convert the first_publish_date column to a type date
books.df$first_publish_date <- mdy(books.df$first_publish_date)
books.df$first_publish_date

# Create a new column called "year" by extracting from the year column in the first_publish_date
books.df$year <- year(books.df$first_publish_date)
books.df$year

# Reducing the dataset to only include books dated from 1990 to 2020
books.df_filtered <- books.df %>%
filter(year >= 1990, year <= 2020)
books.df_filtered

# Removing the following columns
books_new.df <- books.df_filtered %>%
select(-publish_date, -edition, -characters, -price, -genres, -setting, -isbn)
books_new.df

# Filtering for books fewer than 1200 pages
books_new.df_filtered <- books_new.df %>%
filter(pages < 1200)
books_new.df_filtered

# Show a long view of the filtered dataset
glimpse(books_new.df_filtered)

# Show a breakdown of the statistics of the dataset
summary(books_new.df_filtered)
