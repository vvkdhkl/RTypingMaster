#db query to load filtered keystroke data
query = "select child.ORDERID, child.TEST_SECTION_ID, child.LETTER, child.KEYCODE, 
(child.PRESS_TIME-parent.PRESS_TIME) as LATENCY from KEYSTROKES_ORDERED as child, 
(select ORDERID, PRESS_TIME, TEST_SECTION_ID from KEYSTROKES_ORDERED where KEYCODE=32) as parent 
where child.ORDERID=parent.ORDERID+1 
and child.PRESS_TIME-parent.PRESS_TIME>0 
and child.PRESS_TIME-parent.PRESS_TIME<5000
and child.TEST_SECTION_ID=parent.TEST_SECTION_ID
and child.KEYCODE >64 
and child.KEYCODE<91;"

space_letter = read.dbi.ffdf(query, dbConnect.args = list(drv = dbDriver("MySQL"),
                                                            dbname="typingmasterreaddb",
                                                            username="vvkdhkl",
                                                            password=.rs.askForPassword("password: "),
                                                            host="127.0.0.1",
                                                            VERBOSE=TRUE))

ffsave(letter_space, "/home/vvkd/Desktop/RTypingMaster/ff_data/savedff/lsff")