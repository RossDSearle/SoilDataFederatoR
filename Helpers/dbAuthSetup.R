library(RSQLite)
library(DBI)

projectRoot <- 'C:/Users/sea084/Dropbox/RossRCode/Git/soilsDataFederator'
dbPathSoilsFed <- paste0(projectRoot, "/DB/soilsFederator.sqlite")

source(paste0(projectRoot, '/DB/dbHelpers.R'))

conn <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed)


sql <- 'CREATE TABLE AuthGroups (
    GroupName TEXT NOT NULL
                 PRIMARY KEY
                 UNIQUE
                 COLLATE NOCASE
);'
res <- dbSendStatement(conn, sql)

sql <- 'CREATE TABLE AuthAccess (
    GroupName TEXT COLLATE NOCASE,
    access    TEXT COLLATE NOCASE,
    FOREIGN KEY (
        GroupName
    )
    REFERENCES AuthGroups (GroupName) 
);'
res <- dbSendStatement(conn, sql)

sql <- 'CREATE TABLE AuthUsers (
    usrID     TEXT NOT NULL
                   PRIMARY KEY
                   UNIQUE
                   COLLATE NOCASE,
    FirstName TEXT COLLATE NOCASE,
    Surname   TEXT COLLATE NOCASE,
    Organisation TEXT COLLATE NOCASE,
    GroupName TEXT,
    Pwd       TEXT,
    FOREIGN KEY (
        GroupName
    )
    REFERENCES AuthGroups (GroupName) 
);'

res <- dbSendStatement(conn, sql)



