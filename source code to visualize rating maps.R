#install necessary libraries for the first time 
#(you may remove the install code below as no need to install again afterwards)
install_github("tradingeconomics/tradingeconomics/R/tradingeconomics")

# load the tradingeconomics library
library(tradingeconomics)
login('put your own API key from Tradingeconomics')

#get data in dataframe format from tradingeconomics & save it as a table 
tradingeconomics::getCreditRating(outType = "df") %>% mutate(TE = as.numeric(as.character(TE))) -> cr_table

#cleaning data (remove na values)
df <- cr_table %>% na.omit()

#install library echharts4r for the first time 
#(you may remove the install code below as no need to install afterwards)
install.packages("echarts4r")

#load the echarts4r library
library(echarts4r)
# In my opinion, it works better than echarty

#visualize the map with TE Score (its a rating made by tradingeconomics)
df %>%
  e_charts(Country) %>%
  e_map(TE, map = "world",
        zoom = 1.2,
        roam = TRUE,
        scaleLimit = list(
          min = 1.2,   # Don't allow zooming out past 1x (original size)
          max = 2.5   # Allow zooming in up to 10x
        ),
        center = c(120, -2), # centered on Indonesia!
        itemStyle = list(
          emphasis = list(
            areaColor = NULL  # Set to NULL to inherit color from visualMap on hover
          )
        )
  ) %>% 
  e_visual_map(
    TE,
    min = min(df$TE),
    max = max(df$TE),
    text = c("High", "Low"),
    orient = "vertical",
    left = "right",
    top = "center",
    inRange = list(color = c("red", "yellow", "#91cc75"))  # gradient coloring
  ) %>%
  e_title(
    text = "Sovereign Rating Map",
    subtext = "Based on Trading Economic (TE) Score"
  ) %>%
  e_tooltip(trigger = "item")

# I want to make the ratings label more intuitive
# So I map the fitch grades into numbers (scores)
fitch_scale <- c(
  "AAA" = 100, "AA+" = 95, "AA" = 90, "AA-" = 85,
  "A+" = 80, "A" = 75, "A-" = 70,
  "BBB+" = 65, "BBB" = 60, "BBB-" = 55,
  "BB+" = 50, "BB" = 45, "BB-" = 40,
  "B+" = 35, "B" = 30, "B-" = 25,
  "CCC" = 20, "CC" = 15, "C" = 10,
  "RD" = 0, "D" = 0
)

# I want to visualize countries with Fitch ratings, so I clean the data first
# as some countries don't have fitch rating)
df_fitch <- df %>%
  mutate(Fitch_Score = ifelse(Fitch %in% names(fitch_scale),
                              fitch_scale[Fitch], NA))

# OPTIONAL STEP to adjust legend
# I want the legend to read as Fitch label not score so I need to remap the score back
# JavaScript mapping: numeric back to rating label for informative legend
js_formatter <- htmlwidgets::JS("
  function (value) {
    const scale = {
      100: 'AAA', 95: 'AA+', 90: 'AA', 85: 'AA-',
      80: 'A+', 75: 'A', 70: 'A-',
      65: 'BBB+', 60: 'BBB', 55: 'BBB-',
      50: 'BB+', 45: 'BB', 40: 'BB-',
      35: 'B+', 30: 'B', 25: 'B-',
      20: 'CCC', 15: 'CC', 10: 'C',
      0: 'D/RD'
    };
    return scale[value] || Math.round(value);
  }
")

#visualize the map of Fitch with adjusted legend
df_fitch %>%
  e_charts(Country) %>%
  e_map(Fitch_Score, map = "world",
        zoom = 1.2,
        roam = TRUE,
        scaleLimit = list(
          min = 1.2,   # Don't allow zooming out past 1x (original size)
          max = 2.5   # Allow zooming in up to 10x
        ),
        center = c(120, -2), # centered on Indonesia!
        itemStyle = list(
          emphasis = list(
            areaColor = NULL  # Set to NULL to inherit color from visualMap on hover
          )
        )
  ) %>% 
  e_visual_map(
    Fitch_Score,
    min = min(df$Fitch_Score),
    max = max(df$Fitch_Score),
    text = c("High", "Low"),
    orient = "vertical",
    left = "right",
    top = "center",
    inRange = list(color = c("red", "yellow", "#91cc75")),  # gradient coloring
    formatter = js_formatter #OPTIONAL STEP; you may remove this line
  ) %>%
  e_title(
    text = "Sovereign Rating Map",
    subtext = "Based on Fitch Rating"
  ) %>%
  e_tooltip(trigger = "item")

