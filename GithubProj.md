Taylor Swift
================
2025-09-18

``` r
# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(viridis)
```

<!-- # Install devtools if not already installed -->
<!-- install.packages("devtools") -->
<!-- # Install ggradar from GitHub -->
<!-- devtools::install_github("ricardo-bion/ggradar") -->

``` r
# Load ggradar package
library(ggradar)
```

``` r
taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
```

## Introduction

The Taylor Swift datasets were created by W. Jake Thompson. It provides
a comprehensive collection of Taylor Swift’s music, featuring her
official albums, singles and EPs. We will mainly explore the first
dataset, taylor_album_songs, which includes many numerical audio
variables from Spotify for all her songs from her studio albums. We will
focus on the variables - album_name, energy, danceability, acousticness,
valence, key_mode and duration_ms. The album_name variable labels the
album names. The variables energy, danceability, acousticness and
valence are all Spotify ratings. The ratings vary from 0 to 1 with 0
being the least and 1 being the most in terms of the respective
variable. Additionally, the key_mode variable is the key of the song.
Lastly, the duration_ms tells us the duration of the song. The datasets
allow for an in-depth exploration of Taylor Swift’s music and evolution.

Our group will explore the question, “How has the style of Taylor
Swift’s songs evolved across her career eras?” We will mainly explore
the dataset, taylor_album_songs. We will explore the musical
characteristics and duration of her songs to uncover patterns and
insights that have shaped and reflected her musical journey. We define
career eras as the chronological order in which Taylor Swift’s albums
were originally released.

## Data Cleaning & Summary

``` r
eras_order <- c("Taylor Swift", "Fearless (Taylor's Version)", "Speak Now", "Red (Taylor's Version)", "1989", "reputation", "Lover", "folklore", "evermore", "Midnights")
```

``` r
taylor_album_songs <- taylor_album_songs %>% 
  select(album_name, energy, danceability, acousticness, valence,key_mode, duration_ms) %>% 
  drop_na() %>% 
  distinct() %>% 
  mutate(album_name=factor(album_name, levels = eras_order)) %>%
  arrange(album_name)

glimpse(taylor_album_songs)
```

    ## Rows: 191
    ## Columns: 7
    ## $ album_name   <fct> Taylor Swift, Taylor Swift, Taylor Swift, Taylor Swift, T…
    ## $ energy       <dbl> 0.491, 0.877, 0.417, 0.777, 0.482, 0.805, 0.578, 0.629, 0…
    ## $ danceability <dbl> 0.580, 0.658, 0.621, 0.576, 0.418, 0.589, 0.479, 0.594, 0…
    ## $ acousticness <dbl> 0.57500, 0.17300, 0.28800, 0.05100, 0.21700, 0.00491, 0.5…
    ## $ valence      <dbl> 0.425, 0.821, 0.289, 0.428, 0.261, 0.591, 0.192, 0.504, 0…
    ## $ key_mode     <chr> "C major", "G major", "A# major", "A major", "F major", "…
    ## $ duration_ms  <dbl> 232107, 173067, 203040, 199200, 239013, 207107, 248107, 2…

The cleaned dataset focuses only on selected columns, album_name,
energy, key_mode, acousticness, danceability, valence, duration_ms,
removes any NAs and duplicated rows (if any) to avoid misleading and
incorrect data interpretations and arranges the albums according to the
chronological eras order.

Here, we defined the eras order using eras_order, a vector of Taylor
Swift’s albums arranged based on the chronological order in which the
**original** albums were released, instead of just ordering by the album
release date (album_release) given in the dataset.

This is as in the dataset, while most are her original albums, the
albums “Fearless (Taylor’s version)” and “Red (Taylor’s version)” are
her re-released versions, where most of the songs are the same songs in
the original album - just re-recorded so that she owns the new version -
as well as some additional songs she did not release at the time the
original album was released but was still made during that time. Since
the songs were made back in 2008 and 2012 respectively, the musical
style is from those years, and not 2021 when they were re-recorded.
Thus, since we are analysing Taylor’s song style evolution rather than
the technical differences of the re-recordings, we arranged them by
their original release date, placing them 2nd and 4th respectively,
instead of by the re-release date in 2021 which would set them back in
8th and 9th respectively instead (Fitzgerald, 2024).

This ensures that when the albums are displayed on the x-axis of the
plot, they accurately reflect her musical evolution over the years.

## Visualization 1:

Visualization 1 is a line plot with points for the average song duration
for each era.

Dataset used: taylor_album_songs; It contains information about each
song in her various albums, including song duration (duration_ms) in
milliseconds and album name (album_name).

Variables used: album_name, to get the album names (eras) duration_ms,
to get the average song duration for each era

A line plot with points is ideal for this visualization because:  
1. A line plot inherently suggests a progression over time, thus making
it well-suited to show the changes in average song duration across
Taylor Swift’s eras.  
2. By connecting the points with lines, the rise and fall of average
song durations becomes more obvious.  
3. The points on the line plot emphasize each individual album, making
it easy to see the average song duration for each one. Without points,
the specific duration for each album might be less clear, and individual
album differences would be harder to distinguish.

``` r
avg_duration_album <- taylor_album_songs %>%
  mutate(album_name = factor(album_name, levels = eras_order)) %>%
  # Convert album_name into a factor and ordering it according to eras_order will allow the plot to follow Taylor Swift’s discography sequence.
  group_by(album_name) %>%
  # Group by album_name groups the data by each album.
  summarise(avg_duration = mean(duration_ms/60000, na.rm = TRUE)) 
  #  Calculate the average song duration for each album, by converting milliseconds to minutes (divide duration_ms by 60000), and na.rm = TRUE removes any missing values during the calculation. 
```

``` r
ggplot(avg_duration_album, aes(x = album_name, y = avg_duration, group = 1)) + 
  geom_line() +
  geom_point(aes(fill = avg_duration), size = 4, shape = 21, color = "black") +
  # line plot with points, with album_name on the x axis, and avg_duration on the y-axis.
  scale_y_continuous(limits = c(3,5)) +
  # Set the y-axis limits to focus on average song durations between 3 and 5 minutes.
  scale_fill_gradient(low = "lightpink", high = "red", name = "Average Duration") +
  # Use a colour gradient for the points, where lighter shades indicate shorter average durations and darker shades indicate higher average durations.
  labs(title = "Average Song Duration across Taylor Swift's Eras",
       x = "Era",
       y = "Average Duration (Minutes)") +
  geom_label(aes(label = round(avg_duration, 2)), vjust = -1, size = 3, fontface = "bold") +
  # Display each album’s average duration above the points to enhance readability.
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for better readability.
```

<img src="GithubProj_files/figure-gfm/unnamed-chunk-7-1.png" width="80%" style="display: block; margin: auto;" />

## Visualization 2:

Our 2nd visualization is a radar chart that shows Taylor Swift’s musical
evolution across her different eras, grouped by their genres: Country,
Pop transition, Alternative and Modern.

- Create genre groupings with slight adjustments to the data handling

``` r
taylor_data <- taylor_album_songs %>%
  mutate(
    # Add a small jitter to values to prevent exact overlaps
    danceability = danceability + runif(n(), 0, 0.01),
    energy = energy + runif(n(), 0, 0.01),
    acousticness = acousticness + runif(n(), 0, 0.01),
    valence = valence + runif(n(), 0, 0.01),
    # Clamp values to ensure they stay within 0-1
    across(c(danceability, energy, acousticness, valence), 
           ~pmax(0, pmin(1, .))),
    genre = case_when(
      album_name %in% c("Taylor Swift", "Fearless (Taylor's Version)") ~ "Country",
      album_name %in% c("Speak Now", "Red (Taylor's Version)", "1989", "reputation", "Lover") ~ "Pop Transition",
      album_name %in% c("folklore", "evermore") ~ "Alternative",
      album_name == "Midnights" ~ "Modern",
      TRUE ~ "Other"
    )
  ) %>%
  select(genre, danceability, energy, acousticness, valence) %>%
  na.omit()
```

- Calculate mean values

``` r
era_means <- taylor_data %>%
  group_by(genre) %>%
  summarise(across(c(danceability, energy, acousticness, valence), 
                  ~mean(., na.rm = TRUE))) %>%
  # Ensure all values are positive
  mutate(across(-genre, abs))
```

- Reshape data for faceted plotting

``` r
era_means_long <- era_means %>%
  pivot_longer(
    cols = -genre,
    names_to = "characteristic",
    values_to = "value"
  )
```

- Create the visualization

``` r
ggplot(era_means_long, aes(x = genre, y = value, fill = genre)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~characteristic, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(
    "Alternative" = "#FFA500",
    "Country" = "#FF69B4",
    "Modern" = "#1E90FF",
    "Pop Transition" = "#8B008B"
  )) +
  coord_flip() +
  labs(
    title = "Taylor Swift's Musical Evolution",
    subtitle = "Musical characteristics across four major career phases",
    y = "Value (0-1 scale)",
    x = "",
    caption = "Data: Spotify API via Taylor Swift R Package"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )
```

<img src="GithubProj_files/figure-gfm/unnamed-chunk-11-1.png" width="80%" style="display: block; margin: auto;" />

## Visualization 3:

Visualization 3 focuses on finding out how the key mode of Taylor
Swift’s songs evolved through her eras. The variables used are:  
1. album_name, to get the Taylor Swift Era.  
2. key_mode, to get the key_mode of each song per Era album.

Here, a heat map is ideal in answering the question as:  
1. Both variables are categorical.  
2. It is a relatively large data set with key_mode having 18 different
categories.  
3. By using a color gradient, it is easy and intuitive for viewers to
understand and compare the differences in use of key modes across the
Eras, with the added bonus of making the large amount of data more
visually appealing.

Additionally, viridis color scheme is used to increase accessibility by
accounting for colorblindness.

Finally, we use percentage of songs per album with that key mode instead
of just count of songs to normalize the data.

``` r
df3 <- taylor_album_songs %>%
  group_by(album_name, key_mode) %>%
  #count songs with each key_mode per album
  summarise(count = length(album_name)) %>% 
  ungroup() %>%
  #fill in blank cells
  complete(album_name = eras_order, key_mode, fill = list(count = 0.00)) %>%
  group_by(album_name) %>%
  #get percentage of songs per album with each key_mode
  mutate(keymode_songs_percentage = count/sum(count)*100) 

#make sure eras are ordered correctly
df3$album_name <- factor(df3$album_name, levels = eras_order)

ggplot(df3, aes(x=album_name, y=key_mode, fill = keymode_songs_percentage)) +
  geom_tile(color= 'white') +
  geom_text(aes(label = round(keymode_songs_percentage, 2)), size =2, color = 'white') +
  labs(title = "Heatmap of Key Mode of Songs Percentage across Taylor Swift's Eras",
       x = "Taylor Swift Era",
       y = "Key Mode",
       fill = "Percentage of Songs") +
  scale_fill_viridis(option="O") + #viridis to account for colorblindness
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        title = element_text(size=9),
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
```

<img src="GithubProj_files/figure-gfm/Visualization 3-1.png" width="80%" style="display: block; margin: auto;" />

## Discussion

### Visualization 1 Analysis

Visualization 1 shows a chronological view of the evolution of Taylor
Swift’s music in terms of the average song duration for each album. By
arranging her albums in original release order on the x-axis and
plotting the average song duration on the y-axis, we can observe trends
and shifts in her music over time.

The line plot with points shows that Taylor’s first few albums, namely
“Taylor Swift” and “Fearless (Taylor’s Version), have moderate average
song durations around the 3.5-4.5 minutes range. Albums such as “Speak
Now” and “Red (Taylor’s Version)” show a noticeable increase in average
song duration, peaking at around 4.68 minutes for “Red (Taylor’s
Version)”. This may reflect Taylor’s exploration of storytelling in her
music, where she dives deeper into complex emotional narratives and
themes. A slight decline in average song duration is visible in “1989”,
“reputation” and “Lover”, with the average song duration dropping to as
low as 3.44 minutes. This period could likely mark her transition to
more pop-oriented music, often associated with shorter song lengths
which are typically concise and catchy, designed for mainstream appeal
and broader accessibility. The average song duration in albums
“folklore” and “evermore” increase as the album songs embrace indie and
folk elements, possibly indicating her shift towards more introspective
and elaborate compositions, as well as her desire to experiment beyond
commercial pop genres.

The use of a colour gradient for the points visually emphasises changes
in the average song duration, with darker shades representing higher
averages. This intuitive colour gradient allows viewers to quickly
identify albums with longer or shorter average song durations.
Furthermore, adding labels for each point enhances readability, allowing
the viewer to see specific average song durations. The rotated x-axis
labels also ensure clarity in reading the various albums, especially
since some of the albums are lengthy.

### Visualization 2 Analysis

The faceted bar chart reveals clear musical transitions across Swift’s
career eras. The Alternative era (folklore/evermore) stands out
dramatically with the highest acousticness (~0.8), reflecting a strong
return to stripped-down, organic instrumentation, while maintaining
moderate energy and valence (~0.4), characteristic of its indie-folk
influence. Her Pop Transition period shows high danceability (~0.6) and
energy (~0.6) but the lowest acousticness (~0.20), marking her shift to
electronic pop production. The Country era maintains relatively balanced
characteristics with notably high energy (~0.6) and consistent levels of
danceability (~0.55) and valence (~0.4), typical of country-pop
crossover style. The Modern era (Midnights) appears to strike a middle
ground, with moderate-to-high values across all metrics, showing a sound
that synthesizes elements from her previous periods while maintaining
relatively high danceability (~0.6) similar to her pop era.

### Visualization 3 Analysis

For Visualization 3, we used a heat map with the main aim of finding out
how the key mode of Taylor Swift’s songs changed through her eras. From
the heat map, it can be seen that Taylor Swift generally used **3 main
key modes** the most across her eras- **D major, G major and C major**.
This is evident in how her earlier eras of ‘Taylor Swift’ and ‘Fearless
(Taylor’s Version)’ saw relatively high usage of D major (20% and 19.23%
of songs respectively). Her next 3 eras had G major as the highest key
mode of songs percentage in each album - 23.53%, 30% and 31.25% of songs
for ‘Speak Now’, ‘Red (Taylor’s Version)’ and ‘1989’ respectively;
though it should be noted that there was a tie for ‘Speak Now’, where
23.53% of songs in said era used E major too. Following which, her
‘reputation’ era saw a return to D major, with 26.67% of songs. Her
later eras then usually used C major the most, with relatively high
percentages of 38.89%, 17.65%, 23.53% and 20% for the ‘Lover’,
‘folklore’, ‘evermore’ and ‘Midnights’ eras respectively, though the
‘Midnights’ era also saw a return to G major with 25% of songs.

Generally, her **least used key mode** is **G minor** and **B major**,
with both only appearing once in the ‘evermore’ era, at a relatively low
percentage of 5.88% each (equivalent to 1 song). Additionally, the
**variety** of key modes used across her eras- defined as the number of
unique key modes used per era- averages at 9.2 key modes used per era.
It should be noted that ‘Folklore’ had the greatest variety with 13
different key modes used, and ‘1989’, ‘reputation’ and ‘Midnights’ share
the lowest variety with 7 different key modes used. Finally, Taylor’s
use of **minor key modes** also remains relatively **constant and low**,
with only 1-2 used per era. In fact, ‘1989’ used no minor key modes at
all.

### Overall Pattern

Overall, Taylor Swift’s songs have generally become shorter, less
energetic and slightly more emotionally-’negative’ (e.g. sadder,
angrier) across her eras. In terms of the technical aspect, the key mode
of her songs has changed from D major initially, to G major and finally,
C major in later eras.

This can be seen in how the **duration** of her songs has **generally
decreased** despite fluctuations in between eras - It initially
increased from 3.57mins in ‘Taylor Swift’ to a peak of 4.68mins in
‘Speak Now’, before generally decreasing to 3.47mins in ‘Midnights’
(Visualization 1). The **overall Energy and Valence** of her songs has
also **decreased**, from 0.65 for the Country Genre to 0.5 for the
Modern Genre and around 0.4 for the Country Genre to 0.3 for the Modern
Genre respectively. Meanwhile, **overall Danceability** has remained
**constant** around 0.5-0.6 (Visualization 2). Finally, Taylor used the
**major** key mode the most **across all eras**, from **D major to G
major and finally, C major**. She initially using D major the most from
‘Taylor Swift’ to ‘Fearless(Taylor’s Version)’, to G major from ‘Speak
Now’ to ‘1989’, before transitioning back to D major and C major in
‘reputation’, and continuing this use of C major for a large proportion
of her songs from ‘Lover’ to ‘Midnights’ (Visualization 3).

## References

1.  Data source:
    <https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-17/readme.md>
2.  Complete a data frame with missing combinations of data. tidyr.
    (n.d.). <https://tidyr.tidyverse.org/reference/complete.html>
3.  Fitzgerald, T. (2024, July 10). Every Taylor Swift album in order of
    release. Forbes.
    <https://www.forbes.com/sites/entertainment/article/taylor-swift-albums/>
4.  GeeksforGeeks. (2023, June 8). Create Heatmap in R Using ggplot2.
    <https://www.geeksforgeeks.org/create-heatmap-in-r-using-ggplot2/>
5.  Rotating and spacing axis labels in ggplot2. Stack Overflow. (2009,
    August 8).
    <https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2>
6.  Rotating x-axis labels and changing theme in ggplot2. Stack
    Overflow. (2019, February 6).
    <https://stackoverflow.com/questions/54550444/rotating-x-axis-labels-and-changing-theme-in-ggplot2>
7.  Rudis, B., Ross, N., & Garnier, S. (2024, January 28). Introduction
    to the viridis color maps.
    <https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html/>
