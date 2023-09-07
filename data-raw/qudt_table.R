## code to prepare `qudt_table` dataset goes here

turtle_url  <- "https://raw.githubusercontent.com/qudt/qudt-public-repo/master/vocab/unit/VOCAB_QUDT-UNITS-ALL-v2.1.ttl"
turtle_file <- tempfile()

try(
  download.file(
    url      = turtle_url,
    destfile = turtle_file,
    method   = "curl"
  )
)

turtle_qudt <- rdflib::rdf_parse(
  doc    = turtle_file,
  format = "turtle"
)

sparql <- '
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX qudt: <http://qudt.org/schema/qudt/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX smf: <http://topbraid.org/sparqlmotionfunctions#>
PREFIX fn: <http://www.w3.org/2005/xpath-functions#>
PREFIX unit: <http://qudt.org/schema/qudt/unit>

SELECT
	?unit
	?label
	# ?description
	# ?ucumCode
	?hasDimensionVector
	?conversionMultiplier
	(lang(?label) as ?lang)
	# (Datatype(?description) as ?Datatype)

WHERE {

	?unit rdfs:label ?label .
	# OPTIONAL { ?unit dcterms:description ?description . }
  # OPTIONAL { ?unit qudt:ucumCode ?ucumCode . }
  OPTIONAL { ?unit qudt:hasDimensionVector ?hasDimensionVector . }
  OPTIONAL { ?unit qudt:conversionMultiplier ?conversionMultiplier . }

	FILTER ((lang(?label) = "en-us") || (lang(?label) = "en"))
	# FILTER (datatype(?description) != qudt:LatexString)

} ORDER BY ?unit
'

qudt_table <- rdflib::rdf_query(
  rdf   = turtle_qudt,
  query = sparql
)

qudt_table <- qudt_table |>
  dplyr::mutate(name = stringr::str_extract(unit, "[\\w.-]+$"))

usethis::use_data(qudt_table, overwrite = TRUE)
