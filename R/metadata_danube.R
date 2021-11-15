# Prepare Metadata ####
# Markus Bauer


### Packages ###
library(here)
library(tidyverse)
library(EML)
library(emld)
#remotes::install_github("ropenscilabs/emldown", build = F)
library(emldown)
#remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

### Start ###
rm(list = ls())
setwd(here("data/raw"))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Collect metadata ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Methods and units #####################################################################################

methods_file <- here("data/text/methods.odt")
methods <- set_methods(methods_file)

EMLassemblyline::view_unit_dictionary() # List of standard units, which should be used in metadata file

custom_units <- bind_rows(
  data.frame(id = "milligramPerDezigram", 
             unitType = "massPerMass", 
             parentSI = "gramPerGram", 
             multiplierToSI = 0.00001, 
             description = "milligram of element per 100 gram soil"),
  data.frame(id = "millimeterSquaredPerMilligram", 
             unitType = "specificArea", 
             parentSI = "meterPerGram", 
             multiplierToSI = 1, 
             description = "square millimeters per milligram")
  )

unitList <- set_unitList(custom_units)


### 2 Raw data #####################################################################################

### a data_raw_sites  -------------------------------------------------------------------------------------------
setwd(here("data/raw"))
attributes <- read_csv("data_raw_sites_metadata.csv") %>%
  select(-type, -factor)

col_classes <- read_csv("data_raw_sites_metadata.csv") %>%
  select(type)


side <- c(
  water = "water side of the dike",
  land = "land side of the dike"
)
exposition <- c(
  north = "north exposition",
  south = "south exposition"
)

factors <- bind_rows(
  data.frame(
    attributeName = "side",
    code = names(side),
    definition = unname(side)
  ),
  data.frame(
    attributeName = "exposition",
    code = names(exposition),
    definition = unname(exposition)
  )
  )

attributeList_raw_sites <- set_attributes(attributes, 
                                            factors, 
                                            col_classes = col_classes
                                          )
)


physical_raw_sites <- set_physical("data_raw_sites.csv")

### 3 Processed data #####################################################################################

### a data_processed_sites  -------------------------------------------------------------------------------------------
attributes <- read_csv("data_processed_sites_metadata.csv")

position <- c(
  m = "middle part of the slope",
  u = "upper part of the slope",
  l = "lower part of the slope"
)

factors <- bind_rows(
  data.frame(
    attributeName = "position",
    code = names(position),
    definition = unname(position)
  )
)

attributeList_processed_sites <- set_attributes(attributes, 
                                      factors, 
                                      col_classes = c("character", 
                                                      "Date", 
                                                      "factor", 
                                                      "character",
                                                      "numeric", 
                                                      "numeric")
)

physical_processed_sites <- set_physical("data_raw_sites.csv")


### 4 Put data table together #####################################################################################

dataTable <- list(
  list(
    entityName = "data_raw_sites.csv",
    entityDescription = "environmental raw data of the sites",
    #physical = physical_raw_sites,
    attributeList = attributeList_raw_sites
  ),
  #list(
    #entityName = "data_processed_sites.csv",
    #entityDescription = "environmental processed data of the sites",
    #physical = physical_processed_sites,
    #attributeList = attributeList_processed_sites
  #)
)


### 5 Contact #####################################################################################

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany")

creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus", 
    surName = "Bauer"
  ),
  positionName = "PhD student",
  organizationName = "Technical University of Munich",
  address = address,
  electronicMailAddress = "markusbauer@mailbox.org",
  phone = "0049-152-56391781",
  id = "https://orcid.org/0000-0001-5372-4174"
)

associatedParty <- list(
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Jakob", 
      surName = "Huber"
    ),
    role = "Researcher",
    organizationName = "Technical University of Munich",
    electronicMailAddress = "jakob.huber@posteo.de"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Johannes", 
      surName = "Kollmann"
    ),
    role = "Professor",
    organizationName = "Technical University of Munich",
    address = address,
    electronicMailAddress = "jkollmann@wzw.tum.de",
    phone = "0049-8161-714144",
    id = "https://orcid.org/0000-0002-4990-3636"
  )
)

contact <- 
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    onlineUrl = "DOI address to the database"
  )


### 6 Temporal and spatial coverage #####################################################################################

geographicDescription <- "Danube dikes near Deggendorf"

coverage <- set_coverage(
  begin = "2017-06-01", end = "2021-07-31",
  sci_names = list(list(
    Subdivision = "Spermatophytina"
  )),
  geographicDescription = geographicDescription,
  west = 12.58996, east = 13.1162,
  north = 48.90389, south = 48.67502,
  altitudeMin = 309, altitudeMaximum = 315,
  altitudeUnits = "meter"
)


### 7 Description #####################################################################################

pubDate = "2022"

title = "Danube old dikes"

abstract <- "Not written yet"

keywordSet <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list("rivers",
                   "vegetation dynamics",
                   "restoration")
  ),
  list(
    keywordThesaurus = "own vocabulary",
    keyword = list("beta diversity",
                   "temperate grassland",
                   "dike")
  )
)

intellectualRights <- "CC-BY-4.0: https://creativecommons.org/licenses/by/4.0/deed.en"



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B finalize EML ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataset <- list(
  title = title,
  pubDate = pubDate,
  creator = creator,
  associatedParty = associatedParty,
  intellectualRights = intellectualRights,
  abstract = abstract,
  keywordSet = keywordSet,
  coverage = coverage,
  contact = contact,
  methods = methods,
  dataTable = dataTable,
  additonalMetadata = list(metadata = list(
    unitList = unitList
  ))
  )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
  )

setwd(here())
write_eml(eml, "METADATA.xml")
eml_validate("METADATA.xml")

render_eml("METADATA.xml", open = T, outfile = "METADATA.html", publish_mode = F)

