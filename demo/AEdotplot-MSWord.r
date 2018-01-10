data(AEdata, package="HH")

AE1 <- HH::AEdotplot(AE ~ nAE/nTRT, groups = TRT, data = AEdata)
AE2 <- HH::AEdotplot(AE ~ nAE/nTRT | OrgSys, groups = TRT, data = AEdata)

AE1.docx <- MSWord(AE1) ## defaults to device="png", filetype="docx"
AE2.docx <- MSWord(AE2) ## defaults to device="png", filetype="docx"

system(paste("open", AE1.docx))
system(paste("open", AE2.docx))


AE1.html <- MSWord(AE1, filetype="html") ## defaults to device="png"
AE2.html <- MSWord(AE2, filetype="html") ## defaults to device="png"

system(paste("open", AE1.html))
system(paste("open", AE2.html))
