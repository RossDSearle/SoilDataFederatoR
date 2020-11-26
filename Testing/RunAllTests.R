
source('C:/Users/sea084/Dropbox/RossRCode/Git/TernLandscapes/APIs/SoilDataFederatoR/Testing/runFunctionTests.R')


test_Functions(types='LAB', showResults=F, nrows=5, API=T)


test_Functions(types='MORPH', showResults=F, nrows=5, API=T)


test_Functions(types='LOCATIONS', showResults=F, nrows=5, API=T)


test_Functions(types='ALL', showResults=T, nrows=5, API=T)
