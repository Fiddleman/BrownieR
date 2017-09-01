# -- Workflow for analysing physio data for one subject --

# Loading Data
load("tests/testthat/datafortestingphysio/expdata.RData") # unusal import process
class(dataset.all) <- "exp" # unusal import process, as it would be normally a csv files
physio174  <- read.csv2(file = "tests/testthat/datafortestingphysio/physio_sub_0101.csv", sep = ";")

# Create Marker File (per Subject)
markerfile174 <- createMarkerFile.exp(data = dataset.all, subject = "174", min(physio174$ReceiveTime))
# Save markerfile
write.table(markerfile174, 
            file ="tests/testthat/datafortestingphysio/marker174.csv",
            row.names=FALSE,
            na="",
            col.names=FALSE,
            sep=";",
            quote = FALSE) 

# If necessary, bring inputs in proper order:
inputorder <-c(EKG = "Input2", BVP = "Input1", EDA ="Input3")
physio174 <- transformInputs(physio174, inputorder)

# Start now creating the unisens data for the specified subject 174
createUnisensData(data = physio174,
                  subjectname = "174",
                  destinationDirectory = "tests/testthat/datafortestingphysio/",
                  nameUnisensDataFolder = "01_unisens_data",
                  nameTrimmedUnisensDataFolder = "02_unisens_trimmed",
                  pathMarkerFile = "tests/testthat/datafortestingphysio/marker174.csv")

# For Plotting a ECG graph:
ecg_setTriggersDataOpenAnslab(destFolder = "tests/testthat/datafortestingphysio/02_unisens_trimmed", subject = "174")

# And finally, creating the heartrate per screen table:
ecg_createHrTimelineFile(unisensDestDataFolder = "tests/testthat/datafortestingphysio/02_unisens_trimmed", subject = "174", outfilename = "hr174event.csv")
  #ecg_createHrTimelinFile has bug which I cannot fix (yet), maybe it's because of the data. 

