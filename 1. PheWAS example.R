# Install.packages for PheWAS analysis
install.packages("devtools")
install.packages(c("dplyr","tidyr","ggplot2","MASS","meta","ggrepel","DT"))
devtools::install_github("PheWAS/PheWAS")


vignette("PheWAS-package")  #Open the help document for this R package
library(PheWAS)                        #Load Package
set.seed(1)                                 #Set random seeds
ex=generateExample()             #Generate test data


#Extract three parts from the list 
id.vocab.code.count=ex$id.vocab.code.count
genotypes=ex$genotypes        #Genotype file
id.sex=ex$id.sex                        #Gender, as a covariate
#Create phecode table - translate code
phenotypes=createPhenotypes(id.vocab.code.count, aggregate.fun=sum, id.sex=id.sex)
#Run PheWAS
results=phewas(phenotypes,genotypes,cores=1)   
#Plotting
phewasManhattan(results, annotate.angle=0,title="My Example PheWAS Manhattan Plot")
phewasManhattan(results,annotate.angle=0,title="Example PheWAS Manhattan Plot",
                point.size=3.5,annotate.size=3,annotate.only.largest=T,
                OR.direction=T)




#nsform ICD9CM and ICD10CM codes into phecodes
mapCodesToPhecodes(id.vocab.code.count)
# add the description for phecodes
addPhecodeInfo(results)



