# Final-projectECON4970
# Motivation for this Project
The motivation for this project comes from my coursework in Development Economics and International Business Operations. These classes made me curious about how different types of economic activity actually contribute to growth in developing countries. More specifically, I wanted to understand the role of foreign direct investment (FDI) compared to small and medium enterprises (SMEs). I was interested in seeing whether growth is more strongly connected to foreign investment coming into a country or to the development of local, domestic businesses.

# Research Question
Which has a stronger relationship with economic growth in developing countries: foreign direct investment (FDI) or SME activity?

# Where I Found My Data
The data used in this project was obtained from the World Bank Open Data website.

# Data Used in This Analysis
In this project, I used three main variables:

GDP per capita growth (%) -> used to measure economic growth
FDI net inflows (% of GDP) -> used to measure foreign investment
New business density -> used as a proxy for SME activity

Data links:
GDP per capita growth: https://data360.worldbank.org/en/indicator/WB_WDI_NY_GDP_PCAP_KD_ZG?view=trend&average=WLD
FDI net inflows: https://data360.worldbank.org/en/indicator/WB_WDI_BX_KLT_DINV_WD_GD_ZS?view=trend&average=WLD
New business density: https://data360.worldbank.org/en/indicator/WB_WDI_IC_BUS_NDNS_ZS?view=trend&average=WLD

# Figures and Tables Generated During My Analysis
Descriptive Statistics Table

<img width="743" height="465" alt="table" src="https://github.com/user-attachments/assets/8539820e-ad07-4400-9d09-33f2815a66bf" />

This table shows summary statistics for GDP growth, FDI, and SME activity, grouped by high and low FDI countries. On average, countries with higher FDI tend to have slightly higher GDP growth. At the same time, SME activity is somewhat higher in low FDI countries, which suggests that local business activity may play a different role depending on the structure of the economy.

Figure 1: FDI vs. GDP Growth

<img width="2100" height="1500" alt="fdi gdpgrowth" src="https://github.com/user-attachments/assets/944b6c03-a3c0-4cfa-a38f-0976629558ed" />

This graph shows the relationship between FDI and GDP per capita growth. There is a slight positive trend, meaning higher FDI is generally associated with higher growth. However, the points are very spread out, so this emphasizes that the relationship is not very strong.

Figure 2: SME Activity vs. GDP Growth

<img width="2100" height="1500" alt="sme gdpgrowth" src="https://github.com/user-attachments/assets/302d0945-87f7-44c2-a615-147a2bfdb683" />

This graph shows the relationship between SME activity and GDP growth. The trend here is mostly flat, with no strong positive relationship. This suggests that higher levels of business creation do not always directly lead to higher short-term economic growth.

# Summary of Analysis Results
Overall, the results show that FDI has a slightly stronger relationship with economic growth compared to SME activity. However, both relationships are relatively weak, which suggests that growth depends on more than just these two factors. While FDI shows a small positive trend with growth, SME activity does not show a clear pattern in this data. This could mean that the impact of SMEs on growth may take longer to show up or may depend on other factors like government policy, infrastructure, or overall economic conditions.

# Code Used for the Analysis
setwd("C:/Users/cclark138/Downloads")

install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)

################################################################################

#Read in the 3 variable data sets

list.files()

gdp = read_excel("gdpgrowth.xlsx")
fdi = read_excel("fdi.xlsx")
sme = read_excel("WB_WDI_IC_BUS_NDNS_ZS.xlsx")

###################### Cleaning the Data #######################################

#GDP df cleaned by pivot longer, time period of 2006-2022, narrowing countries,
#renaming, and removing unnecessary columns
countries = c(
  "South Africa", "Kenya", "Nigeria", "Ghana",
  "Egypt, Arab Rep.", "Morocco", "Tunisia", "Ethiopia",
  "Tanzania", "Uganda", "Senegal", "Rwanda",
  "India", "Brazil", "Indonesia", "Mexico"
)

gdp_clean = gdp |>
  select(`Country Name`, `Country Code`, "2006":"2022") |>
  rename(country = `Country Name`,
         country_code = `Country Code`) |>
  pivot_longer(
    cols = "2006":"2022",
    names_to = "year",
    values_to = "gdp_growth"
  )|> 
  mutate(
    year = as.numeric(year),
    gdp_growth = as.numeric(gdp_growth)) |>
  filter(country %in% countries)

#Clean FDI the same way
fdi_clean = fdi |>
  select(`Country Name`, `Country Code`, "2006":"2022") |>
  rename(country = `Country Name`,
         country_code = `Country Code`) |>
  pivot_longer(
    cols = "2006":"2022",
    names_to = "year",
    values_to = "fdi"
  ) |> 
  mutate(
    year = as.numeric(year),
    fdi = as.numeric(fdi)) |>
  filter(country %in% countries)

#Clean SME Proxy data
sme_clean = sme |>
  select("REF_AREA", "TIME_PERIOD", "OBS_VALUE") |>
  rename(country_code = "REF_AREA",
         year = "TIME_PERIOD",
         sme = "OBS_VALUE"
) |> 
  mutate(
    year = as.numeric(year),
    sme = as.numeric(sme)) |>
  filter(year >= 2006 & year <= 2022)

########################## Merge the data ######################################
df = gdp_clean |>
  left_join(fdi_clean, by = c("country", "country_code", "year")) |>
  left_join(sme_clean, by = c("country_code", "year"))

################### Creating Descriptive Statistics Table ######################
#Create grouping variable for high vs. low FDI
df = df |>
  mutate(
    fdi_group = ifelse(
      fdi > median(fdi, na.rm = TRUE),
            "High FDI", "Low FDI"))


#Create statistics table grouped by FDI level
projecttable = df |>
  group_by(fdi_group) |>
  summarize(
     mean_gdp_growth = mean(gdp_growth, na.rm = TRUE),
     median_gdp_growth = median(gdp_growth, na.rm = TRUE),
     sd_gdp_growth = sd(gdp_growth, na.rm = TRUE),
     min_gdp_growth = min(gdp_growth, na.rm = TRUE),
     max_gdp_growth = max(gdp_growth, na.rm = TRUE), mean_fdi = mean(fdi, na.rm = TRUE),
     median_fdi = median(fdi, na.rm = TRUE),
     sd_fdi = sd(fdi, na.rm = TRUE),
     min_fdi = min(fdi, na.rm = TRUE),
     max_fdi = max(fdi, na.rm = TRUE), mean_sme = mean(sme, na.rm = TRUE),
     median_sme = median(sme, na.rm = TRUE),
     sd_sme = sd(sme, na.rm = TRUE),
     min_sme = min(sme, na.rm = TRUE),
     max_sme = max(sme, na.rm = TRUE)
  )     

#Export table
write.csv(projecttable, "finalprojecttable.csv")

############# Create Scatter Plot of FDI and GDP Growth ########################
fdi_scatter = ggplot(df, aes(x = fdi, y = gdp_growth)) +
  geom_point(aes(color = fdi_group)) +
  theme(legend.position = "bottom") +
  labs(
    x = "FDI Net Inflows (% of GDP)",
    y = "GDP per Capita Growth (%)"
  ) +
  geom_smooth(se = TRUE) +
  theme_classic()

fdi_scatter

#Save graph
ggsave("fdi.gdpgrowth.png", fdi_scatter, width = 7, height = 5)

############# Create Scatter Plot of SMEs and GDP Growth #######################
sme_scatter = ggplot(df, aes(x = sme, y = gdp_growth)) +
  geom_point(aes(color = fdi_group)) +
  theme(legend.position = "bottom") +
  labs(
    x = "New Business Density",
    y = "GDP per Capita Growth (%)"
  ) +
  geom_smooth(se = TRUE) +
  theme_classic()

sme_scatter

#Save graph
ggsave("sme.gdpgrowth.png", sme_scatter, width = 7, height = 5)
