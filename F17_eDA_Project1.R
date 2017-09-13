devtools::install_github("datadotworld/dwapi-r", build_vignettes = TRUE)

saved_cfg <- data.world::save_config("eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Omplbm5pZmVyY2h1bmciLCJpc3MiOiJhZ2VudDpqZW5uaWZlcmNodW5nOjozZTNiMTAzMy1kYzMxLTRmZTAtYTM4NS0yNDFjMjVmNzY2NTUiLCJpYXQiOjE0ODQ2OTcyNDUsInJvbGUiOlsidXNlcl9hcGlfd3JpdGUiLCJ1c2VyX2FwaV9yZWFkIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.wObx6AHihRFAOixo6S51tLFadtGEsOoJNndHSIPajvSdXshuRzzemuLUxuMvU4vSLjWu4g2PesjXPjPqggdRWA")
data.world::set_config(saved_cfg)

vignette("quickstart", package = "data.world")

require(dplyr)
require(data.world)

project <- "https://data.world/tommywilczek/elections-data"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM finalproject_ElectionsData"),
  dataset = project
)
summary(df)
sdf = dplyr::select(df, total_population, votes, votes16_trumpd, votes16_clintonh, votes16_johnsong, votes16_steinj, rep16_frac, dem16_frac,at_least_bachelor_s_degree, at_least_high_school_diploma, less_than_high_school, graduate_degree, white_not_latino_population, african_american_population, native_american_population, asian_american_population, population_some_other_race_or_races, latino_population, management_professional_and_related_occupations, service_occupations, sales_and_office_occupations, farming_fishing_and_forestry_occupations, construction_extraction_maintenance_and_repair_occupations, production_transportation_and_material_moving_occupations, adult_obesity, diabetes, uninsured, unemployment, diabetes_2) %>% sample_frac(.1) # The dplyr::select function was used to select only continuous variables so that the pairs() function works in default mode. The sample_frac() function returns a 10% sample of the data so that pairs doesn't choke.
pairs(sdf)
plot(votes~total_population,df)
fitVotesTotalPopulation = lm(votes~total_population,data=df)
fitVotesTotalPopulation
summary(fitVotesTotalPopulation)
abline(fitVotesTotalPopulation,col="red")
names(fitVotesTotalPopulation)
confint(fitVotesTotalPopulation)

fitVotesTotalPopulation2 = lm(votes~total_population + poly(total_population, 2),data=df)
fitVotesTotalPopulation2
summary(fitVotesTotalPopulation2)
points(total_population, fitted(fitVotesTotalPopulation2), col="blue", pch=10) ##THIS ISN'T WORKING
confint(fitVotesTotalPopulation2)

fitDemUninsured <- lm(dem16_frac~uninsured, df)
fitDemUninsured
summary(fitDemUninsured)

fitVotesStein <- lm(votes~votes16_steinj, df)
fitVotesStein
summary(fitVotesStein)

fitDemGraduateDegree <- lm(dem16_frac~graduate_degree, df)
fitDemGraduateDegree
summary(fitDemGraduateDegree)
