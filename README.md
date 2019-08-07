# soilsDataFederator

The SoilDataFederator is a web API that brings together soil site data from a range of disparate data sources. It allows the user to query soils data stores across Australia in a consistent manner and the data returned is a consistent format. Users do not need to know the details of each of the individual data stores structures and querying mechanisms. The data in the system is historical soil survey data and is composed of both morphological description data and Laboratory analysis data.

The SoilDataFederator web API is designed to be as simple as possible. Users can query API end point to return information about

    * Providers - which organisations are contributing data.
    * Properties - which soil properties are available in the system.
    * PropertyGroups - defined groupings of Properties (a convenience for querying)

and then return data using the

    * SoilData end point.

The code for the SoilDataFederator is written in R

More detailed information about the SoilDataFederator can be found at http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/help/index.html
