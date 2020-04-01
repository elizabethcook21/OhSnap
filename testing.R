#testing'
  library(readxl)
  library(writexl)
  library(jsonlite)
file = 'CBC.xlsx'
fileDF <- read_excel(file)
fileDF_colNames = colnames(fileDF)
inData = c(1,2,3,4,5,6,9,8,9,0,11,12,13,14,15,16)
inData
inData = c("12.4.2",inData)
inData
as.vector(inData)
fileDF = rbind(fileDF,inData, stringsAsFactors = FALSE)
colnames(fileDF) = fileDF_colNames
write_xlsx(fileDF, 'filedf.xlsx')
fileDF2 = read_excel('filedf.xlsx')
bob = fileDF2[1,-1]
bob = c("12.3.2", bob)
names(bob) = NULL
bob2 = c("13,33,3",1,2,33,4,5,6,7,8,22,0,11,12,13,14,15,16)
names(bob2) = fileDF_colNames
fileDF = rbind(fileDF, bob2)

View(fileDF2)
View(fileDF)
colnames(fileDF)
cbc = c('WBC', "RBC", "HCT", "MCV", "MCHC", "PLT", "RDW-SD", "NEUT", "LYMPH", "MONO", "EO", "BASO")
cbc = c("Date", cbc)
cbc
values = c(121,12,1,1,123,1,1,2,2,1,1,21)
data.frame(colnames(fileDF), cbc, values)
View(data.frame(Expected = colnames(fileDF)[-1]))

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = c(sqrt(-1), letters[1:9]),
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

# try updating big to a value not in the dropdown
rhandsontable(DF, rowHeaders = NULL, width = 550, height = 300) %>%
  hot_col(col = "big", type = "numeric") %>%
  hot_col(col = "small", type = "autocomplete", source = letters,
          strict = FALSE)

DF_na = data.frame(integer = c(NA, 2:10), 
                   logical = c(NA, rep(TRUE, 9)), 
                   character = c(NA, LETTERS[1:9]),
                   factor = c(NA, factor(letters[1:9])),
                   date = c(NA, seq(from = Sys.Date(), by = "days", 
                                    length.out = 9)),
                   stringsAsFactors = FALSE)

DF_na$factor_ch = as.character(DF_na$factor)
DF_na$date_ch = c(NA, as.character(seq(from = Sys.Date(), by = "days", 
                                       length.out = 9)))

bob = rhandsontable(DF_na, width = 550, height = 300)
bob
View(fromJSON(bob$x$data))
