devtools::load_all('/home/iulian/datashield/dsSwissKnife')
devtools::load_all('/home/iulian/datashield/dsSwissKnifeClient')
devtools::load_all('/home/iulian/datashield/dsHelpers')
devtools::load_all('/home/iulian/datashield/dsHelpersClient')
loc<- dssCreatePseudoServers(servers = 2)
logindata <- read.delim('/home/iulian/datashield/logindata_test.txt')
logindata$server <- c('remote1', 'remote2')
opals <- datashield.login(loc, logindata)

