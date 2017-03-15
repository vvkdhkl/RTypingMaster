query = "select d.PARTICIPANT_ID, a.KEYCODE, a.nxKEYCODE, AVG(IKI) AS AVG_IKI, 
  STDDEV(IKI) AS SD_IKI FROM         
  (SELECT child.TEST_SECTION_ID AS TSID, 
  child.KEYCODE AS KEYCODE, parent.KEYCODE AS nxKEYCODE,  
  CASE WHEN parent.PRESS_TIME-child.PRESS_TIME BETWEEN 0 AND 5000 THEN parent.PRESS_TIME-child.PRESS_TIME ELSE NULL END as IKI  
    from KEYSTROKES_COMPLETE child, 
    KEYSTROKES_COMPLETE parent 
      WHERE child.ORDERID=parent.ORDERID-1 AND child.TEST_SECTION_ID=parent.TEST_SECTION_ID 
      AND (parent.KEYCODE=32 or parent.KEYCODE between 65 and 90) 
      and (child.KEYCODE=32 or child.KEYCODE between 65 and 90)) a, 
  TEST_SECTIONS d WHERE a.TSID = d.TEST_SECTION_ID GROUP BY d.PARTICIPANT_ID, a.KEYCODE, a.nxKEYCODE;"

library(data.table)
bigram_iki = data.table(dbGetQuery(conn, query))
save(bigram_iki, file = "/home/vvkd/Desktop/RTypingMaster/bigram_raw.Rda")

bigram_iki$keypair = paste(bigram_iki$KEYCODE, bigram_iki$nxKEYCODE)
bigram_iki_c = dcast.data.table(bigram_iki, PARTICIPANT_ID ~ keypair, value.var = "AVG_IKI")

save(bigram_iki_c, file = "/home/vvkd/Desktop/RTypingMaster/bigram_data.Rda")


"SELECT a.* FROM (SELECT child.TEST_SECTION_ID AS TSID,    child.KEYCODE AS KEYCODE, parent.KEYCODE AS nxKEYCODE,     CASE WHEN parent.PRESS_TIME-child.PRESS_TIME BETWEEN 0 AND 5000 THEN parent.PRESS_TIME-child.PRESS_TIME ELSE NULL END as IKI       from KEYSTROKES_COMPLETE child,      KEYSTROKES_COMPLETE parent        WHERE child.ORDERID=parent.ORDERID-1 AND child.TEST_SECTION_ID=parent.TEST_SECTION_ID        AND (parent.KEYCODE=32 or parent.KEYCODE between 65 and 90)        and (child.KEYCODE=32 or child.KEYCODE between 65 and 90)) a, TEST_SECTIONS_COMPLETE b WHERE a.TSID=b.TEST_SECTION_ID AND b.PARTICIPANT_ID=78207 ORDER BY a.KEYCODE, a.nxKEYCODE;"