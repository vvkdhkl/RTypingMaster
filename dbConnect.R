library(RMySQL)
conn <- dbConnect(MySQL(), user = 'vvkdhkl', 
                  password = .rs.askForPassword("password: "), host = '127.0.0.1', dbname = 'typingmasterreaddb')