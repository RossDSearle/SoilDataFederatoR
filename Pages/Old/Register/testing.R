
del <- 'delete from AuthUsers where usrID = "ross.searle@gmail.com"'

authcon <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed, flags = SQLITE_RW)

res <- dbSendStatement(authcon, del)
nr <- dbGetRowsAffected(res)
print(nr)
dbDisconnect(authcon)


authcon <- dbConnect(RSQLite::SQLite(), dbPathSoilsFed, flags = SQLITE_RW)
t <- dbReadTable(authcon, "AuthUsers")
print(t)


tKey <- 'fm4kMUvG3N'
email <- 'ross.searle@gmail.com'
lastName <- 'Bob'
firstName <- 'Brown'
organisation <- 'Asylum'
