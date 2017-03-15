library(data.table)
library(XML)
library(RMySQL)

conn <- dbConnect(MySQL(), user = 'vvkdhkl', 
                  password = .rs.askForPassword("password: "), host = '127.0.0.1', dbname = 'typingmasterreaddb')

query1 = "select PARTICIPANT_ID from PARTICIPANTS_COMPLETE WHERE NUM_TESTS>=15;"
participants = as.data.frame(dbGetQuery(conn, query1))

query4 = "select * from SENTENCES;"
sentences = as.data.frame(dbGetQuery(conn, query4))

for (p in 1:100) {
  query2 = paste("select TEST_SECTION_ID, SENTENCE_ID, USER_INPUT from TEST_SECTIONS_COMPLETE WHERE PARTICIPANT_ID =",participants[p,],";")
  testsecs = as.data.frame(dbGetQuery(conn, query2))
  
  query5 = paste("select * from KEYSTROKES_COMPLETE WHERE TEST_SECTION_ID =",testsecs[1,1],";")
  keystrokes2 = as.data.frame(dbGetQuery(conn, query5))
  
  xmlT = xmlTree()
  xmlT$addNode("TextTest", attrs = c(version="2.7.2", trials=15, ticks=keystrokes2[1,2]+621260000000000000), close = FALSE)
  
  for (t in 1:nrow(testsecs)) {
    query3 = paste("select * from KEYSTROKES_COMPLETE WHERE TEST_SECTION_ID =",testsecs[t,1],";")
    keystrokes = as.data.frame(dbGetQuery(conn, query3))
    if (nrow(keystrokes)>0) { 
    
    kount = sum((keystrokes[,6] ==32) | (keystrokes[,6] == 8) | ((keystrokes[,6]>=65) & (keystrokes[,6]<=90)))
      
    xmlT$addNode("Trial", attrs = c("number"=t, "testing"="false", "entries" = kount), close = FALSE)
    xmlT$addNode("Presented",gsub("[^[:alnum:] ]", "", sentences[sentences$SENTENCE_ID==as.numeric(testsecs[t,2]),2]), close = TRUE)
   
    for (k in 1:nrow(keystrokes)) {
     
        if ((keystrokes[k,6] ==32) || (keystrokes[k,6] == 8) || ((keystrokes[k,6]>=65) && (keystrokes[k,6]<=90))) {
                    xmlT$addNode("Entry", attrs = c(char = ifelse(keystrokes[k,4]=="BKSP", "&#x8;",
                    ifelse((keystrokes[k,6] ==32) || ((keystrokes[k,6]>=65) && (keystrokes[k,6]<=90)),keystrokes[k,4], " ")),
                    value=ifelse((keystrokes[k,6] ==32) || (keystrokes[k,6] == 8), keystrokes[k,6], keystrokes[k,6] + 32),
                   ticks=keystrokes[k,2]+621260000000000000, seconds=(keystrokes[k,2]+621260000000000000)/10000000), 
                   cdata = TRUE, close = TRUE)
      }}
     
    
    
    xmlT$addNode("Transcribed", gsub("[^[:alnum:] ]", "", testsecs[t,3]))
    xmlT$closeNode("Trial")
  }}
  saveXML(xmlT$doc(), file = gsub(" ","",paste("/home/vvkd/Desktop/RTypingMaster/errorxmls/",participants[p,1],".xml")))
}