module SampleTables where

emptyTable :: [[String]]
emptyTable = []

t1 = map words $ [
    "first last gender salary",
    "Alice Allen female 82000",
    "Bob Baker male 70000",
    "Carol Clarke female 50000",
    "Dan Davies male 45000",
    "Eve Evans female 275000"
    ]

t2 = map words $ [
    "asdjhf last poiuzx zxc mnb first ewrt ert salary ghj",
    "adfdcvzdcbv 4563456345 zxcvzxcv rf wvdsfv ergsdcvzdcv aertvcvb ertefv dcea zcb",
    "asdgasdgf zcxberg zxc eyzb aert zcxvb 34563456 zcxbv aert zcvb",
    "awet udghj 3456 adsfg xvcbn sery zxb srytj zdsfb zetrh",
    "dfgadfbfdbae df adefgadb erb 34563456 erhgaeb zdfbdf aergha zdfbd rt",
    "dfbadfbeaq 3456345 erg zdfbdfb aergaer 3456 bzderh zdfnb sae dafb"
    ]

-- Need one with column names that are used in fields
t3 = map words $ [
    "username password country",
    "alice 1234 NL",
    "bob 4512 US",
    "carol password DE",
    "username password ES",
    "username passw0rd UK"
    ]