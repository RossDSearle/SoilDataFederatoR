

# FLgetData_LawsonGrains <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#  # getData_TERNLandscapes(provider='WAGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_WAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#  #getData_TERNLandscapes(provider='WAGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_ASRIS <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   #getData_TERNLandscapes(provider='WAGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_TernLandscapes <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_TERNLandscapes( observedProperty, observedPropertyGroup)
# }
# FLgetData_SAGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#  # getData_TERNLandscapes(provider='SAGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_VicGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#  # getData_TERNLandscapes(provider='VicGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_TasGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#  # getData_TERNLandscapes(provider='TasGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_NSWGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#  # getData_TERNLandscapes(provider='NSWGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_QLDGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#
#   getData_QLDGovernment( observedProperty, observedPropertyGroup)
#   #getData_TERNLandscapes(provider='QLDGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_NTGovernment <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   #getData_TERNLandscapes(provider='NTGovernment', observedProperty, observedPropertyGroup)
# }
# FLgetData_CSIRO <- function(observedProperty=NULL, observedPropertyGroup=NULL){
#   getData_ASRIS( observedProperty, observedPropertyGroup)
# }


getDataFunctions <- c(LawsonGrains=getData_LawsonGrains,
                      QLDGovernment=getData_QLDGovernment,
                      ASRIS=FLgetData_ASRIS,
                      TERNLandscapes=getData_TERNLandscapes,
                      TERNSurveillance=getData_TERNSurveillance,
                      #WAGovernment=FLgetData_WAGovernment,
                      #NSWGovernment=FLgetData_NSWGovernment,
                      #VicGovernment=FLgetData_VicGovernment,
                      #QLDGovernment=FLgetData_QLDGovernment,
                      #SAGovernment=FLgetData_SAGovernment,
                      #TasGovernment=FLgetData_TasGovernment,
                      #NTGovernment=FLgetData_NTGovernment,
                      CSIRO=getData_ASRIS)

