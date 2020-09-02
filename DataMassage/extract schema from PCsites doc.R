#######   need to swith to 32 bit version of R


Sys.setenv('JAVA_HOME' = 'C:/Program Files (x86)/Amazon Corretto/jdk1.8.0_252/jre')

library(rJava)
library(tabulizer)
library(dplyr)

p <- 'C:/Users/sea084/Desktop/SITES-v2_National Schema.pdf'

out <- extract_tables(p)
saveRDS(out, 'c:/temp/shema.rds')

schema <- readRDS('c:/temp/shema.rds')
str(schema)
schema[[10]]
