
email <- 'echo "This is the main body of the mail <a href=https://www.abc.net.au/news/> ABC </a>"| mail -s "$(echo -e "This is the subject\nContent-Type: text/html")" ross.searle@csiro.au -r TheSoilManager@soils-discovery.csiro.au'

system(email)
