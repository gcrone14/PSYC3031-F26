##### Mini-Activity 3 Solutions #####

#' Before beginning addressing the questions,
#' we start by setting up our environment.

# Load libraries
library(psych)
library(here)
library(ez)
library(tidyverse)

# Import data
games <- read_csv(here::here("Data/videogametype.csv"))
glimpse(games) # Inspect data
table(games$hyperactivity) |> names()
table(games$game_type) |> names()

# Clean the data
games_final <- games |>
  mutate(ID = factor(ID),
         hyperactivity = factor(hyperactivity),
         game_type = factor(game_type))
glimpse(games_final)

#' 1)
#' 
#' Since we're given two categorical IVs with a 
#' continuous DV, we'd run a two-way (2x2) ANOVA.
#' 
#' 2) 
#' 
#' The research question might be:
#' "Do hyperactive children tend to have more violent
#' images appear in their dreams, stories, 
#' and pictures than non-hyperactive children, and
#' does this effect change based on whether children
#' play violent or prosocial video games?"
#' 
#' 3) 
#' 
#' We can compute descriptive stats to see what
#' the answer might be.
games_final |>
  group_by(hyperactivity, game_type) |>
  summarise(mean_nviolent_images = mean(nviolent_images),
         sd_nviolent_images = sd(nviolent_images),
         median_nviolent_images = median(nviolent_images))

#' Let's plot this relationship.
games_final |>
  ggplot(aes(x = game_type, y = nviolent_images, 
             fill = hyperactivity)) +
  geom_boxplot() +
  facet_wrap(~hyperactivity) +
  scale_y_continuous(breaks = 1:13) +
  theme_classic() +
  labs(x = "Game type", y = "# of Violent Images",
       fill = "Hyperactivity")

#' And an effect plot of the means
games_final |>
  ggplot(aes(x = game_type, y = nviolent_images, 
             color = hyperactivity, 
             group = hyperactivity)) +
  stat_summary(geom = "point", fun = mean) +
  stat_summary(geom = "line", fun = mean) +
  theme_classic() +
  labs(x = "Game type", y = "# of Violent Images",
       color = "Hyperactivity")


#' These descriptive statistics
#' reveal that violent video games tend to
#' increase the number of violent images
#' children produce regardless of their hyperactivity.
#' 
#' The effect, though, differs based on hyperactivity:
#' For hyperactive children, the effect of violent video games
#' increasing the number of violent images was far stronger
#' within hyperactive children than non-hyperactive
#' children.
#' 
#' Written out more formally (in APA style):
#' 
#' The type of game tends to increase the number of violent
#' images children produced. Hyperactive children playing
#' prosocial games produce an average of four (*SD* = 1.22)
#' violent images, whereas these children playing
#' violent games produce far more (*M* = 11.2, *SD* = 1.30) violent
#' images.
#' 
#' Among non-hyperactive children, those playing prosocial games
#' produce an average of 4.6 (*SD* = 1.14) violent images, while
#' those playing violent video games tend to produce an
#' average of nine (*SD* = 1.58) violent images. 
#' 
#' 4) 
#' 
#' We conduct our two-way ANOVA as follows:
#' 
#' a) Assess assumptions
#' 
#' We start by assessing the
#' normality assumption.
games_final |>
  ggplot(aes(sample = nviolent_images)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(hyperactivity ~ game_type) +
  theme_classic()

#' Based upon the above Q-Q plot, the 
#' close alignment of points to the
#' theoretical QQ-lines suggest
#' normality is met.
#' 
#' (You can also assess this with
#' descriptive stats and/or other
#' visualizations, such as density
#' plots or boxplots.)
#' 
#' The other assumption is homogeneity
#' of variances.
res <- ezANOVA(games_final,
               wid = ID,
               between = hyperactivity:game_type,
               dv = nviolent_images,
               return_aov = TRUE)
res$`Levene's Test for Homogeneity of Variance`

#' The assumption of homogeneity of variances
#' is also met! (I wonder if these data were simulated...)
#' 
#' We will implicitly assume that the other assumptions
#' (e.g., independence, random sampling from a homogenous population)
#' are both met.
#' 
#' b) Conduct the ANOVA
res$ANOVA

#' There is a clear instnace of an interaction effect:
#' the effect of hyperactivity on the number of violent
#' images differes as a function of the nature of the
#' video game children played.
#' 
#' Post-hoc tests:
mod_1 <- aov(nviolent_images ~ hyperactivity*game_type,
             data = games_final)
TukeyHSD(mod_1)$`hyperactivity:game_type` |>
  as.data.frame() |>
  # Arrange results from most positive to most negative
  arrange(desc(diff)) |>
  # Indicate which means are statistically significant
  mutate(is_sig = `p adj` < .05)

#' The post-hoc tests confirm the interaction effect
#' we found earlier. 
#' 
#' 5) 
#' 
#' I ran a two-way ANOVA to determine whether there
#' was a statistically significant interaction
#' effect between hyperactivity (hyperactive vs. not) and
#' whether children played violent or prosocial
#' video games on the number of violent images
#' they generated.
#' 
#' Descriptively, the type of game tended to increase 
#' the number of violent images children produced. 
#' In particular, hyperactive children playing
#' pro-social games produced an average of four (*SD* = 1.22)
#' violent images, whereas these children playing
#' violent games produced far more (*M* = 11.2, *SD* = 1.30) 
#' violent images.
#' 
#' Among non-hyperactive children, children playing prosocial games
#' produced an average of 4.6 (*SD* = 1.14) violent images, while
#' those playing violent video games produced an
#' average of nine (*SD* = 1.58) violent images.
#' 
#' Prior to running the ANOVA, I checked the assumptions
#' of normality and homogeneity of variance, and confirmed
#' that these assumptions were met with close alignment
#' of observed data to Q-Q line and a statistically
#' non-significant Levene's test (*F*(3, 16) = 0.28,
#' *p* = .838), respectively. 
#' The assumptions of the independence of observations
#' and random selection were both assumed.
#' 
#' The two-way ANOVA showed a statistically significant interaction
#' effect, *F*(1, 16) = 5.60, *p* = .03, *ges* = 0.259.
#' 
#' Tukey's honestly significant difference tests were
#' conducted on all of the mean differences. Most mean differences
#' were statistically significant, except for the mean differences
#' between children who had a different hyperactivity
#' level but played the same type of video game.
#' 
#' To illustrate the interaction, hyperactive children
#' playing violent games generated 7.2 more
#' violent images than those playing prosocial games
#' (*p* < .001, 95% CI [4.81, 9.59]). Non-hyperactive
#' children playing violent video games generated
#' 4.4 more violent images than those playing a prosocial
#' game (*p* < .001, 95% CI [2.01, 6.79]).
#' 
#' In conclusion, playing violent video
#' games corresponded with a statistically significant
#' increase in the number of violent images children
#' generated, and this difference was stronger
#' for hyperactive children than
#' non-hyperactive children.
