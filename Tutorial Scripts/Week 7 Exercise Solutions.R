##### Week 7 Exercise Solutions #####
# Load libraries
library(ez)
library(psych)
library(here)
library(tidyverse)

#' 1. Install the cereal 3.csv data into a new R script. You may
#' call this script whatever you wish (e.g., Week7_practice.R). Then,
#' save the cereal data as cereal_dat.
cereal_dat <- read_csv(here::here("Data/cereal 3.csv"))


#' 2. Perform the following data cleaning steps and save the result into 
#' a new data frame called cereal_final. The steps are as follows:
#' 
#' 2a. Select the name, shelf and 
#' rating variables from cereal_dat.
#' 
#' 2b. Change the structure of the shelf variable to a factor 
#' and relabel the levels so that: 
#'   * 1 = Bottom Shelf 
#'   * 2 = Middle Shelf 
#'   * 3 = Top Shelf
#' 
#' 2c. Change the structure of the name variable to a factor.
#' 
#' We do all three stpes in one go:
cereal_final <- cereal_dat |>
  select(name, shelf, rating) |>
  mutate(shelf = factor(shelf,
                        1:3,
                        c("Bottom Shelf", "Middle Shelf",
                          "Top Shelf")
                        ),
         name = factor(name)
         )

#' We verify it works:
cereal_final |> glimpse()


#' 3. Go through the different steps of a One Way ANOVA to determine whether there 
#'    is a difference in the average rating of cereals placed on the bottom, middle, 
#'    and top shelf. Write a brief APA summary of the results.
#' 
#' Prior to doing the ANOVA, we should explore the data!

# Check frequencies of shelf
summary(cereal_final$shelf)

# Check rating descriptives
cereal_final |> 
  select(rating) |>
  describe()

# Check ratings by shelf
cereal_final |> 
  select(rating) |>
  describeBy(cereal_final$shelf)

#' Now let's make plots to visualize this mean difference!
cereal_final |>
  ggplot(aes(x = shelf, y = rating)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Shelf", y = "Rating")

#' It seems that bottom and top shelf cereals are far
#' more favorably rated than middle shelf ones.
#'  
#' Now, let's check the assumptions  
#' 
#' 1) Normality
cereal_final |>
  ggplot(aes(x = rating, fill = shelf)) +
  geom_density(alpha = 0.3, adjust = 2) +
  facet_wrap(~shelf)

#' Normality doesn't really seem to be met here, but
#' it might be the plot choise that makes it seem this way
#' (deviation doesn't appear too bad).
#' We can also check a Q-Q plot (more on this for Week 8)
cereal_final |>
  ggplot(aes(sample = rating)) +
  geom_qq(colour = "dark blue") +
  geom_qq_line(color = "dark red") +
  facet_wrap(~shelf)

#' Good alignment between observed points
#' and the Q-Q line, so seems tentatively met.
#' 
#' 2) Homogeneity of variances
res <- ezANOVA(data = cereal_final,
               wid = name,
               between = shelf,
               dv = rating,
               return_aov = TRUE)

res$`Levene's Test for Homogeneity of Variance`
# OR
res[2]

#' The assumption is met.
#' 
#' Since normality and homogeneity are fine, let's run
#' the two-way ANOVA!
#' 
#' We already actually ran it above, so we'll just pluck
#' out the required output
res$ANOVA

#' F(2, 74) = 4.79, p = .011, so we reject H0 and conclude that
#' there is a mean difference between groups.
#' 
#' We should also run post-hoc tests to see which groups differ
TukeyHSD(x = res$aov)

#' Tukey's HSD test suggests that there is a statistically 
#' significant difference between middle- and bottom-shelf cereals 
#' (bottom shelf has a 11.17 rating increase) and between top shelf 
#' and middle shelf cereals
#' (top shelf has a 10.25 rating increase), but not between top and
#' bottom shelf cereals (they only have a 0.93 difference).
#' 
#' Some useful output for APA summary:
cereal_final |> 
  select(rating) |>
  describeBy(cereal_final$shelf)

res[1]
res[2]



#' APA summary (the ** indicate italics):
#' 
#' I wanted to determine if there was a statistically significant difference
#' between popular cereal brands based upon where they tend to be placed
#' on the shelf in stores (top, middle, or bottom-shelf).
#' 
#' The mean rating for bottom (*M* = 46.15, *SD* = 13.56) and top-shelf cereals 
#' (*M* = 45.22, *SD* = 12.97)
#' was similar, but both were larger than middle-shelf cereals
#' (*M* = 34.97, *SD* = 13.95). Figure 1 (below) depicts the
#' distributional differences between the three shelf placements.
cereal_final |>
  ggplot(aes(x = shelf, y = rating)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Shelf", y = "Rating",
       title = "Figure 1",
       subtitle = "Mean Rating Difference Between Shelf Placement of various Cereals")


#' To test whether the means differed in the population, 
#' I ran a one-way ANOVA. Prior to doing so, however,
#' I verified that several statistical assumptions were met, 
#' such as normality (as indicated by the roughly normal density 
#' curve, and close alignment of values to the theoretical Q-Q line) and
#' homogeneity of variances (Levene's Test: *F*(2, 74) = 0.24, *p* = .79).
#' 
#' The ANOVA showed that there was a statistically significant
#' rating difference between groups, *F*(2, 74) = 4.79, *p* = .011,
#' $\eta^2 = 0.114$. Tukey's HSD tests confirmed that the mean
#' differences between top- and middle- shelf, and bottom- and middle-
#' shelves, were statistically significantly different.
#' 
#' The effect size of interest was raw mean differences,
#' and the difference between middle- and other shelf-placements 
#' was roughly 10 rating units,
#' which is considered quite large. Thus, it appears that middle-
#' shelf cereals tend to have lower ratings compared to top-
#' or bottom-shelf cereals.
