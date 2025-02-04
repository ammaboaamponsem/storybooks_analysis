# Loading the ggplot2 package 
library(ggplot2)

# Create the rating histogram
ggplot(books_new.df_filtered, aes(x = rating)) +
  geom_histogram(binwidth = 0.25, fill = "red") +
  labs(
    title = "Histogram of Book Ratings",
    x = "Rating",
    y = "Number of Books"
  ) +
  theme_bw()

# Install and load the ggthemes package
library(ggthemes)

# Create the horizontal boxplot
ggplot(books_new.df_filtered, aes(x = pages)) +
  geom_boxplot(fill = "magenta", width = 0.4) +
  labs(
    title = "Box Plot of Page Counts",
    x = "Pages"
  ) +
  theme_economist()

# Group the data by publisher and summarize the number of books
summary_books.df <- books_new.df_filtered %>%
group_by(publisher) %>%
summarize(num_books = n())
summary_books.df

#Install and load the tidyr package to use the drop_na() function
library(tidyr)

# Remove rows with NAs
summary_books.df <- summary_books.df %>% drop_na()
summary_books.df

# Remove publishers with fewer than 250 books
summary_books.df <- summary_books.df %>%
filter(num_books >= 250)
summary_books.df

# Order the total number of books in descending order
summary_books.df <- summary_books.df %>%
arrange(desc(num_books))
summary_books.df

# Make the publisher into a factor with levels defined by the current ordering
summary_books.df$publisher <- factor(summary_books.df$publisher, levels = summary_books.df$publisher)
summary_books.df$publisher

# Add a column with cumulative count of books
summary_books.df <- summary_books.df %>%
mutate(cumulative_count = cumsum(num_books))
summary_books.df

# Add a column with relative frequency of books
summary_books.df <- summary_books.df %>%
mutate(relative_frequency = num_books / sum(num_books))
summary_books.df

# Add a column with cumulative relative frequency of books
summary_books.df <- summary_books.df %>%
mutate(cumulative_relative_frequency = cumsum(relative_frequency))
summary_books.df

# Install and load the ggeasy package
library(ggeasy)

# Calculate the cumulative counts and cumulative relative frequency
summary_books.df <- summary_books.df %>%
  arrange(desc(num_books)) %>%
  mutate(
    cumulative_count = cumsum(num_books),
    cumulative_relative_frequency = cumulative_count / sum(num_books)
  )
summary_books.df

# Create the Pareto Chart with cumulative counts and ogive with dots
pareto_chart <- ggplot(summary_books.df, aes(x = reorder(publisher, -num_books), y = num_books)) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_line(aes(x = publisher, y = cumulative_count, group = 1), color = "black") +
  geom_point(aes(x = publisher, y = cumulative_count), color = "black") +  # Add dots
  labs(
    x = "Publisher",
    y = "Number of Books",
    title = "Pareto and Ogive of Publisher Book Counts (1990 - 2020)"
  ) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the Pareto Chart
print(pareto_chart)

# Convert 'year' to numeric
books_new.df_filtered$year <- as.numeric(books_new.df_filtered$year)

# Create the scatter plot 
scatter_plot <- ggplot(books_new.df_filtered, aes(x = pages, y = rating, color = year)) +
  geom_point() +
  labs(
    x = "Pages",
    y = "Rating",
    title = "Scatter Plot of Pages vs. Rating"
  ) +
  scale_color_gradient(low = "black", high = "cyan", breaks = seq(1990, 2020, by = 10)) +
  theme_tufte() # Specify the color range and year key

# Display the Pareto Chart
print(scatter_plot)

# Calculate the count and average rating by grouping the books data frame by year
yearlybooks_summary <- books_new.df_filtered %>%
  group_by(year) %>%
  summarise(
    count = n(),
    avg_rating = mean(rating, na.rm = TRUE)
  ) %>%
  ungroup()
yearlybooks_summary

# Filter the year data from 1990 to 2020
filtered_data <- yearlybooks_summary %>%
  filter(year >= 1990 & year <= 2020)

# Create the line plot
line_plot <- ggplot(filtered_data, aes(x = year, y = count, color = avg_rating)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Total Number of Books Rated Per Year",
    x = "Year",
    y = "Count",
    color = "Average Rating"
  ) +
  scale_color_gradient(low = "black", high = "cyan") +
  theme_excel_new()
line_plot

# Compute the average or sample mean
custom_mean <- function(x) {
  sum(x) / length(x)
}

# Compute the population variance
pop_var <- function(x) {
  n <- length(x)
  mu <- custom_mean(x)
  sum((x - mu)^2) / n
}

# Compute the population standard deviation
sd_var <- function(x) {
  sqrt(pop_var(x))
}

# Example of usage:
values <- c(2, 4, 4, 4, 5, 5, 7, 9)
average <- custom_mean(values)
cat("Average (Sample Mean):", average, "\n")

population_variance <- pop_var(values)
cat("Population Variance:", population_variance, "\n")

population_sd <- sd_var(values)
cat("Population Standard Deviation:", population_sd, "\n")

# Population statistics for book ratings
population_values <- books_new.df_filtered$rating  # Filter out all book ratings

# Compute population average (mean)
population_average <- custom_mean(population_values)
cat("Population Average (Mean):", population_average, "\n")

# Compute population variance
population_variance <- pop_var(population_values)
cat("Population Variance:", population_variance, "\n")

# Compute population standard deviation
population_sd <- sd_var(population_values)
cat("Population Standard Deviation:", population_sd, "\n")

# Create a tibble
results_tibble <- tibble(
  avg_rating = population_average,
  variance = population_variance,
  sd = population_sd
)
results_tibble

# Create three samples of size 100
set.seed(123)  # Set a random seed for reproducibility
sample_size <- 100

sample1 <- books_new.df_filtered %>%
  sample_n(size = sample_size)

sample2 <- books_new.df_filtered %>%
  sample_n(size = sample_size)

sample3 <- books_new.df_filtered %>%
  sample_n(size = sample_size)

# Compute sample statistics
sample1_mean <- custom_mean(sample1$rating)
sample1_variance <- var(sample1$rating)
sample1_sd <- sd(sample1$rating)

sample2_mean <- custom_mean(sample2$rating)
sample2_variance <- var(sample2$rating)
sample2_sd <- sd(sample2$rating)

sample3_mean <- custom_mean(sample3$rating)
sample3_variance <- var(sample3$rating)
sample3_sd <- sd(sample3$rating)

# Create a tibble with sample statistics
sample_stats_tibble <- tibble(
  Sample = c("Sample 1", "Sample 2", "Sample 3"),
  Mean = c(sample1_mean, sample2_mean, sample3_mean),
  Variance = c(sample1_variance, sample2_variance, sample3_variance),
  SD = c(sample1_sd, sample2_sd, sample3_sd)
)

print(sample_stats_tibble)

# Additional visualizations
# Histogram of Book Ratings
ggplot(books_new.df_filtered, aes(x = rating)) +
  geom_histogram(fill = "green", color = "black", bins = 30) +
  labs(
    title = "Distribution of Book Ratings",
    x = "Rating",
    y = "Frequency"
  ) +
  theme_minimal()

# Average Rating by Year
average_rating_by_year <- books_new.df_filtered %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating))

ggplot(average_rating_by_year, aes(x = year, y = avg_rating)) +
  geom_line(color = "orange") +
  geom_point(color = "orange") +
  labs(
    title = "Average Book Rating by Year",
    x = "Year",
    y = "Average Rating"
  ) +
  theme_minimal()
