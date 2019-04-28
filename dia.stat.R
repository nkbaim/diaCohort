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


# length可以不相同，但如果不通，length小的数据，names 完全属于另一个;
# levels必须相同; 
compareTwoClusterClass<-function(c1,c2){
	
	if(!is.vector(c1) | !is.vector(c2)){
		stop('input clusters must be vectors:err1')
	}

	cshort<-vector()
	clarge<-vector()

	if(length(c1)<=length(c2)){
		cshort<-c1
		clarge<-c2
	}else{
		cshort<-c2
		clarge<-c1
	}


	if(!all(names(cshort) %in% names(clarge))){
		stop('names of c1 and c2 not match:err2')
	}

	clarge <- clarge[names(cshort)]

	require('e1071')

	k=length(unique(cshort))

	res<-vector()
	perms<-permutations(k)

	for(p in 1:nrow(perms)){

		temp <- perms[p,]
		sums<-0
		for(i in 1:k){
			cshort.i <- cshort[cshort==i]
			clarge.i <- clarge[clarge==temp[i]]
			overlap.i <- length(intersect(names(cshort.i),names(clarge.i)))
			sums <- sums + overlap.i
		}
		res.temp<-sums/length(cshort)
		res<-c(res,res.temp)
	}

	return(paste0(max(res)*100,"%"))
}
