study.stat.tumor<-function(study.design.file){
	require(dplyr)
	study.design<-read.table(study.design.file,sep="\t",header=T,check.names=F,stringsAsFactors=F)

	sample.stat <- study.design %>% 
					filter(Condition=='Tumor') %>% 
					group_by(Sample_ID) %>% 
					summarise_at(.vars = c('TechReplicate','BioReplicate'),function(x) { length(unique(x))})

	sample.stat<-as.data.frame(sample.stat)

	No.tumorsRuns<-nrow(study.design[study.design$Condition=='Tumor',])
	No.tumors<-nrow(sample.stat)
	No.tumorWithTechRep<-nrow(sample.stat[sample.stat$TechReplicate>1,])
	No.tumorWithBioRep<-nrow(sample.stat[sample.stat$BioReplicate>1,])

	print(paste0('No.tumorsRuns: ',No.tumorsRuns))
	print(paste0('No.tumors: ',No.tumors))
	print(paste0('No.tumorWithTechRep: ',No.tumorWithTechRep))
	print(paste0('No.tumorWithBioRep: ',No.tumorWithBioRep))

}
