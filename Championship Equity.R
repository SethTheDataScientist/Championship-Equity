wider <- pivot_wider(WARData, names_from = position, values_from = PosWAR)

SeasonFeatures <- wider %>%
  select(season, team_name, CB, DI, S, ED, OC, G, OT, TE, WR, QB, LB, HB) %>% 
  group_by(season, team_name) %>%
  summarise(across(everything(), ~ first(na.omit(.))))

seasonSBOdds <- read_csv("Work Files/Salary and Draft Project/seasonSBOdds.csv")

NFL_Conversion_Names <- read_excel("C:/Users/sethl/OneDrive/Excel Files/Football/Data Csv/NFL Conversion Names.xlsm")

SBOdds <- seasonSBOdds %>% 
  left_join(NFL_Conversion_Names, by = c("Team" = "Full Name")) %>% 
  select(Year, Code, Value) %>% 
  left_join(SeasonFeatures, by = c("Year" = "season", "Code" = "team_name"))

formula = lm(Value ~ QB + WR + TE + HB + OC + G + OT + DI + ED + LB + CB + S,
  data = SBOdds)

summary(formula)
 