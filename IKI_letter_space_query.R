#This script assumes a connection to the database has been defined as conn
#Refer to dbconnect.R script for that

#db query to load filtered keystroke data

query = "select child.ORDERID, child.TEST_SECTION_ID, child.LETTER, child.KEYCODE,
(parent.PRESS_TIME-child.PRESS_TIME) as LATENCY from KEYSTROKES_COMPLETE as child, 
(select TEST_SECTION_ID, KEYCODE, ORDERID, PRESS_TIME from KEYSTROKES_COMPLETE) as parent    
where child.ORDERID=parent.ORDERID-1 
and parent.PRESS_TIME-child.PRESS_TIME>0 
and parent.PRESS_TIME-child.PRESS_TIME<2000
and child.TEST_SECTION_ID = parent.TEST_SECTION_ID
and child.KEYCODE >64
and parent.KEYCODE=32
and child.KEYCODE<91;"

letter_space = read.dbi.ffdf(query, dbConnect.args = list(drv = dbDriver("MySQL"),
                                                          dbname="typingmasterreaddb",
                                                          username="vvkdhkl",
                                                          password=.rs.askForPassword("password: "),
                                                          host="127.0.0.1",
                                                          VERBOSE=TRUE))

ffsave(letter_space, "/home/vvkd/Desktop/RTypingMaster/ff_data/savedff/lsff")