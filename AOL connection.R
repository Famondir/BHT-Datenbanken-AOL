# ssh -L 1521:141.64.89.143:1521 s92818@compute.beuth-hochschule.de

library(RODBC)
library(odbc)
library(DBI)

pswd <- readline("Input Password: ")
channel <- odbcConnect("AOLDB", uid="S944521",  pwd=pswd)

con <- dbConnect(odbc::odbc(), "oracledb", UID="samples", PWD= rstudioapi::askForPassword("Samples User Password"))


con <- DBI::dbConnect(odbc::odbc(),
               driver="Oracle in instantclient_12_2",
               Host="localhost",
               Port=1521,
               SVC="rispdb1",
               uid="S944521",
               pwd=pswd)


library(RJDBC)

drv <- JDBC("com.mysql.jdbc.Driver",
            "C:/Users/schae/OneDrive - Berliner Hochschule für Technik/Medieninformatik/2. Semester/Datenbanken/datavirtuality-jdbc_3.0.1.jar",
            identifier.quote="`")
conn <- dbConnect(drv, "jdbc:mysql://localhost/test", "user", "pwd")