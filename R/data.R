#' Most recent case data based on Robert Koch Institute
#'
#' Data as known to the Robert Koch Institute at 2020-09-21.
#'
#' Data is provided by Bundesamt für Kartographie und Geodäsie Robert Koch-Institut
#' under the open data license dl-de/by-2-0.
#' @source https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data
#'
#' @examples
#' # daily cases by reporting date (Meldedatum)
#' rki_new %>%
#'  group_by(Meldedatum) %>%
#'  summarise(cases = sum(AnzahlFall[Neuer.Fall %in% c(0,1)]))
"rki_new"


#' Weather data from the German weather service
#'
#' Includes date, admin code and name for county (Landkreis), latitude, longitude,
#' and the weather variables
#' FM.Windgeschwindigkeit, FX.Windspitze, NM.Bedeckungsgrad,
#' PM.Luftdruck, RSK.Niederschlagshoehe, SDK.Sonnenscheindauer,
#' TGK.Lufttemperatur_5cm_min, TMK.Lufttemperatur,
#' TNK.Lufttemperatur_Min, TXK.Lufttemperatur_Max,
#' UPM.Relative_Feuchte und VPM.Dampfdruck.
#'
#' @source https://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html
"weather_dwd"

#' Population data by Regionaldatenbank
#'
#' Name, admin level, age, total (population), area, and population density for county, state, and Germany.
#'
#' @source https://www.regionalstatistik.de/genesis/online/logon
#' Genesis Tabelle 12111-04-01-4
"regionaldatenbank"


#' List of interventions in Germany
#'
#' Admin level, Bundesland, County, intervention type, start and end of implementation.
#'
#' @source https://docs.google.com/spreadsheets/d/1cmGBMUhBt5y6jwiqaF7lh7VQNMN6D5FCoIltlOzhMfI/edit#gid=0
"intervention.list"


#' Dummy matrix for German interventions
#'
#' Date, location (unit), Bundesland, and 46 rows for interventions. See by ls(interventions).
"interventions"

#' Ratio of traced infectious cases
#'
#'Date, location (unit), Bundesland, and 46 rows for interventions. See by ls(interventions).
"traced"
