# this script will automatically generate a spreadsheet containing sample IDs, CT values, and VOC results
# it requires outputs from Databank, both Amplitude SM servers, and the VOC results file.

require(dplyr)

# combine nuth1 and nuth2 data
amplitude <- rbind(cog_output,cog_output_nuth2)

# Create a new compression plate ID field in NPEX data
# Take ID from results filename
npex_output$CompressionPlateId=substr(npex_output$`Adb result source file`,19,26)

# rename column for consistency between Amplitude and Databank
amplitude = rename(amplitude, CustomerId = CustomerReference)

# Create new dataset, by merging Amplitude and Databank samples that 
# share a barcode and compression plate
all_sample_manager <- merge(amplitude,npex_output,by=c("CustomerId","CompressionPlateId"))
all_sample_manager = subset(all_sample_manager, select = c(CustomerId,CompressionPlateId,DateCompleted,Ms2Ct, NgeneCt, SgeneCt, Orf1abCt) )
all_sample_manager = rename(all_sample_manager, Sample = CustomerId)
 
# flatten VOC data
VOC_short = subset(VOC_output, select = c(Sample,`CH1-Target`,`CH1-Result`) )
lists <- Map(function(x) data.frame(c(x[1,], x[2,-1],x[3,-1],x[4,-1])), split(VOC_short, VOC_short$Sample))
VOC_flat   <- do.call(rbind, lists)
VOC_flat = rename(VOC_flat, E484K=`CH1.Result`, K417N=`CH1.Result.1`,K417T=`CH1.Result.2`, P681R=`CH1.Result.3`)
VOC_short_flat  = subset(VOC_flat, select = c(Sample,E484K,K417N,K417T,P681R) )

# create output dataframe

finalVlookup <- merge(all_sample_manager, VOC_short_flat, by="Sample", all.y=TRUE)
x=c("Sample ID","PCR Plate Barcode","Date Processed on Amplitude","MS2 ct value","N gene ct value","S gene ct value","Orf1ab ct value","E484K","K417N",	"K417T", "P681R")
colnames(finalVlookup) <- x

write.csv(finalVlookup,"/Users/alex/Desktop/output.csv", row.names = FALSE)


vlookup <- data.frame(matrix(ncol = 12, nrow = 0))
x=c("Sample ID","PCR Plate Barcode","Date Processed on Amplitude","MS2 ct value","N gene ct value","S gene ct value","Orf1ab ct value","E484K","K417N",	"K417T", "P681R","Date catalogued")
colnames(vlookup) <- x
