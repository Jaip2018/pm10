# Prepare data for export to CSV

s18temp <- s18[,c(2,14:28,31,32)]
usmtemp <- usm[,c(2,14:28,31,32)]
write.table(s18temp,'data/s18.csv',sep=',')
write.table(usmtemp,'data/usm.csv',sep=',')
rm(s18temp,usmtemp)