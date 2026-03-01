# Filter down the generic OlsonNames() list of timezones to the modern IANA timezones
timeZones <- function() {
  # Get all timezone names
  all_zones <- OlsonNames()
  
  # Filter out common legacy/system zones (e.g., SystemV, legacy links)
  modern_zones <- all_zones[!grepl("^SystemV|^US/|^Canada/|^Brazil/|^Mexico/|^Etc/", all_zones)]
  
  # Further narrow to common, standardized IANA zones
  # This filters for the "Continent/City" structure
  modern_zones <- modern_zones[grepl("/", modern_zones)]
  return(modern_zones)
}

# Get the desired date input using System time as the default, and a timezone which can be changed by the user
getDate <- function(date_input = Sys.time(), timezone = "America/New_York") {
  return(as.Date(date_input, tz = timezone))
}