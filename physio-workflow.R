# -- Workflow for analysing physio data of one subject --

# Loading Data
load("tests/testthat/datafortestingphysio/expdata.RData")
pyhsio0101  <- read.csv2(file = "tests/testthat/datafortestingphysio/physio_sub_0101.csv", sep = ";")

#MarkerFileCreation
a <- min(pyhsio0101$ReceiveTime)
class(dataset.all) <- "exp"
markerfile <- createMarkerFile.exp(data = dataset.all, subject = "0101", a)


initalizeUnisens()

# -- Processing from other physio package
cutData(raw_data, subject, data_folder)
generateTimestampOffsetInput(raw_data, subject, data_folder)
loadRawData(cut_folder, unisense_folder, subject)
insertEventEntry(unisense_folder, subject, markerfile)
insertTimestampOffsetEntry(unisense_folder, subject)
insertOffsetCorrectedEventEntry(unisense_folder, subject)
trimUnisensData(unisense_folder, trimmed_unisens_folder, subject)
runFiltersEcgData(trimmed_unisens_folder, subject)
ecg_setTriggersDataOpenAnslab(trimmed_unisens_folder, subject)
ecg_createHrTimelineFile(trimmed_unisens_folder, subject, paste(subject,".csv", sep = ""))