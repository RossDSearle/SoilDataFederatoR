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
schema[[9]]
length(schema)


for(i in 1:length(schema)){
  df <- schema[[i]]
  write.csv(df, paste0('c:/temp/Tables/', i, '.csv'))
}


outdf <- data.frame(column_name=character(), Domain_name=character(), Description=character(), Data_type=character(), Length=character(), isNull=character(), stringsAsFactors = F)
for (i in 6:46) {
  df <- read.csv(paste0('c:/temp/Tables/', i, '.csv'))
  outdf<-rbind(outdf, df)
}

write.csv(outdf, paste0('c:/temp/Tables/AllFields.csv'))
