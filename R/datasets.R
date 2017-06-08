#' Census at School 500
#'
#' A dataset containing 500 observations from a New Zealand census of school
#' students.
#'
#' @format A data frame with 500 rows and 10 variables:
#' \describe{
#'   \item{cellsource}{the source of cellphone money}
#'   \item{rightfoot}{length of the right foot, mm}
#'   \item{travel}{travel method used to get to school}
#'   \item{getlunch}{how they get their lunch}
#'   \item{height}{height measurement, cm}
#'   \item{gender}{their biological gender}
#'   \item{age}{their age, years}
#'   \item{year}{their school year}
#'   \item{armspan}{their armspan measurement, cm}
#'   \item{cellcost}{money spent on cellphones}
#' }
#' @source \url{http://new.censusatschool.org.nz/}
"census.at.school.500"


#' Gap Minder
#'
#' A dataset containing variables for countries over several years.
#'
#' @format A data frame with 3577 rows and 55 variables:
#' \describe{
#'   \item{Country}{the country name}
#'   \item{Region.Geo}{geographical region}
#'   \item{Continent}{continent name}
#'   \item{Region}{region name}
#'   \item{Year}{year of observation}
#'   \item{Year_cat}{year of observation, as a factor}
#'   \item{AgriculturalLand}{amount of agricultural land}
#'   \item{BodyMassIndex_M}{average BMI for males}
#'   \item{BodyMassIndex_F}{average BMI for females}
#'   \item{Cellphones}{}
#'   \item{ChildrenPerWoman}{}
#'   \item{CO2Emissions}{}
#'   \item{DemocracyScore}{}
#'   \item{EnergyUsePerPerson}{}
#'   \item{Exports}{}
#'   \item{Femalesaged25to54labourforceparticipationrate}{}
#'   \item{Forestarea}{}
#'   \item{GDPpercapita}{}
#'   \item{Governmenthealthspendingperpersontotal}{}
#'   \item{Hightotechnologyexports}{}
#'   \item{Hourlycompensation}{}
#'   \item{Imports}{}
#'   \item{IncomePerPerson}{}
#'   \item{Incomeshareofpoorest10pct}{}
#'   \item{Incomeshareofrichest10pct}{}
#'   \item{Infantmortality}{}
#'   \item{Inflation}{}
#'   \item{Internetusers}{}
#'   \item{LifeExpectancy}{}
#'   \item{Literacyrateadulttotal}{}
#'   \item{Literacyrateyouthtotal}{}
#'   \item{Longtermunemploymentrate}{}
#'   \item{Medianage}{}
#'   \item{MedicalDoctors}{}
#'   \item{Murder}{}
#'   \item{Murderedmen}{}
#'   \item{Murderedwomen}{}
#'   \item{Oilconsumptionperperson}{}
#'   \item{Populationgrowth}{}
#'   \item{Populationtotal}{}
#'   \item{Populationdensity}{}
#'   \item{Poverty}{}
#'   \item{Ratioofgirlstoboysinprimaryandsecondaryeducation}{}
#'   \item{Renewablewater}{}
#'   \item{Residentialelectricityuseperperson}{}
#'   \item{Suicideage15to29}{}
#'   \item{Taxrevenue}{}
#'   \item{Teenfertilityrate}{}
#'   \item{TotalGDPUS}{}
#'   \item{TotalhealthspendingperpersonUS}{}
#'   \item{Tradebalance}{}
#'   \item{Trafficdeaths}{}
#'   \item{UrbanpopulationTotal}{}
#'   \item{Urbanpopulationgrowth}{}
#'   \item{YearlyCO2emission}{}
#' }
"gapminder"


#' SURF Income Survey
#'
#' The NZIS is run annually during the June quarter (April to June),
#' as a supplement to the Household Labour Force Survey (HLFS).
#' All respondents in the HLFS were asked to participate in the NZIS,
#' which provides a snapshot of income levels for people and households.
#' NZIS data gives average weekly income for the June quarter from most sources,
#' including government transfers, investments, self-employment, and wages and salaries.
#'
#' This is a subset of that data.
#'
#' @format A data frame with 200 rows and 8 variables:
#' \describe{
#'   \item{Personid}{a random unique ID number for each record in the Super SURF}
#'   \item{Gender}{male and female}
#'   \item{Qualification}{five categories}
#'   \item{Age}{25 to 64 years in 5-year age bands}
#'   \item{Hours}{total usual weekly hours worked from all wages and salary jobs}
#'   \item{Income}{total usual weekly income from all sources}
#'   \item{Marital}{marital status}
#'   \item{Ethnicity}{six categories}
#' }
#' @source \url{http://www.stats.govt.nz/tools_and_services/university-students/NZIS-Super-SURF.aspx}
"SURFIncomeSurvey_200"
