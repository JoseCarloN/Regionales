data <- tibble(
  country = 
    c("PT", "IE", "GB", "IS",
      
      "NO", "SE", "DK", "DE", "NL", "BE", "LU", "ES", "FR", "PL", "CZ", "AT",
      "CH", "LI", "SK", "HU", "SI", "IT", "SM", "HR", "BA", "YF", "ME", "AL", "MK",
      
      "FI", "EE", "LV", "LT", "BY", "UA", "MD", "RO", "BG", "GR", "TR", "CY",
      
      "RU"),  
  tz = c(rep("UTC", 4), rep("UTC + 1",25), rep("UCT + 2",12), "UTC + 3")
)

# auxiliar variable
data <- data %>% 
  mutate(value = cumsum(!duplicated(tz)))


# now we'll create the dataClasses
dta_clss <- data %>% 
  mutate(value = cumsum(!duplicated(tz))) %>% 
  group_by(tz) %>% 
  summarise(value = unique(value)) %>% 
  arrange(value) %>% 
  rename(name = tz, from = value) %>% 
  mutate(to = from + 1) %>% 
  list_parse()

hcmap(
  map = "custom/europe",
  data = data, 
  joinBy = c("iso-a2","country"),
  name = "Time zone",
  value = "value",
  tooltip = list(pointFormat = "{point.x} {point.tz}"),
  dataLabels = list(enabled = TRUE, format = "{point.}")
) %>%
  hc_colorAxis(
    dataClassColor = "category",
    dataClasses = dta_clss
  ) %>% 
  hc_title(text = "Europe Time Zones")