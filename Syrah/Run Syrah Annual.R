#*******************************************************************************
#Project Name: SYRAH ANNUAL - Running the complete model
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 11.4.13
#
#Purpose: Run the Bristol Bay, Alaska Run Reconstructions in one script.
#
#  1) Create ADMB input files
#  2) Call ADMB for East and West Size
#  3) Plot Output
#  4) Update variance parameters for error distribution
#  5) Re-run ADMB from from previous parameter estimates
#  6) Plot Output and create brood and return tables
#
#*******************************************************************************
#Notes:
#  A)
#  
#*************************************************************************************************
require(PBSmodelling)
require(R2admb)
require(openxlsx)
require(reshape2)
require(ggplot2)
require(beanplot)
require(mcmcplots)
require(RColorBrewer)


############## CONTROL SECTION ##############
#Set working directory
# wd <- "O:/DCF/REG2/BBsalmon/Run Reconstruction/2019/Syrah Annual_updated_22Oct18/Syrah Annual"

# wd <- "/Users/curryc2/Documents/Curry's SYRAH Work/Bristol-Bay-Run-Recon"
# NOTE IF YOU SET WORKING DIRECTORY TO THE GITHUB REPO THIS SHOULD WORK FINE...
#wd <- "C:/Projects/Bristol_Bay/Bristol-Bay-Run-Recon-master"
wd <- file.path("C:","Projects","Bristol_Bay","Bristol-Bay-Run-Recon")
# DEFINE VERSION OF SYRAH
#model.name <- "Syrah"

# wd <- file.path("C:","Projects","Bristol_Bay","Bristol-Bay-Run-Recon-master")

# PLEASE SET WORKING DIRECTORY TO: "Project Directory"
#   Session > Set Working Director > To Project Directory
wd <- getwd()

# DEFINE VERSION OF SYRAH
# model.name <- "Syrah"

model.name <- "Syrah_v1"
#model.name <- "Syrah_v2"

#############################################
setwd(wd)
##### Source Necessary Files #####
#Create ADMB input
source(file.path("R","Annual Reconstruction.r"))
#Plot output
source(file.path("Syrah","outputFiles","Plot Annual Output.r"))
#plot.all(plot.years=plot.years, plot.side=plot.side, cross=cross)
#create.all(plot.years=plot.years, plot.side=plot.side)

#Helper Functions
source(file.path("Syrah","Syrah Helper Functions.r"))
#move(); cleanup(); both take side and year as inputs


#Compile ADMB CODE
#setup_admb("/Applications/ADMB-11 Terminal.app/Contents/admb-11")
# Sys.setenv(ADMB_HOME='usr/local/bin/ADMB')
# Sys.setenv(PATH='/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin')

# if(mac==TRUE) {
#   system("admb syrah.tpl") #Compiling .tpl file into executable
# }else {
#   shell("admb syrah.tpl") #Compiling .tpl file into executable
# }

#using R2admb
# compile_admb("syrah", verbose=TRUE)
#compile_admb(model.name, verbose=TRUE)

#######################################################################################################
year <- 2022
gen.dat <- TRUE  #Flag for Whether GSI data are available
#######################################################################################################
#Plotting parameters
plot.years <- 1963:year
cross <- 1.25 
#######################################################################################################
#WEST SIDE
phz.run=1; phz.sel=1; phz.avail=1;
temp_sigmaCat=0.1; temp_sigmaEsc=0.05 # For Second Fit

do.west <- TRUE
if(do.west==TRUE) {
  side <- 'west'
  
  #Cleanup
  cleanup(side=side, year=year, wd=wd)

  #Create ADMB Input
  if(gen.dat==TRUE) {
    fixed.avail=rep(-1,3) #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
  }else {
    #Get Average Availability
    west.prelim <- c(1965,1977,1980,1982,1983,1985,1993,1995,1999,2006:(year-1))
    avg.avail <- avg.Avail.prelim(side='west', prelim.years=west.prelim, wd=wd)$avail.avgs
    
    fixed.avail=avg.avail #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
  }
  create.SYRAH.annual.input(side=side, district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            # fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            # fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            fixed.avail=fixed.avail,
                            loc.prefix=paste(wd, "/Syrah/datFiles/", sep=""),
                            read.outs=FALSE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.5, wd=wd) 
  #Run ADMB 1st Round                          
  setwd(file.path(wd, "Syrah"))
  
  #With R2admb
  run_admb(model.name, extra.args=paste("-ind datFiles/WestSide_",year,".dat -rs -nox", sep=''), verbose=TRUE)

  
  move(side=side, year=year, wd=wd, model.name=model.name)
  #Plot


  #Create ADMB Input
  if(gen.dat==TRUE) {
    fixed.avail=rep(-1,3) 
  }else {
    fixed.avail=avg.avail #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
  }
  create.SYRAH.annual.input(side=side, district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            # fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            #fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            fixed.avail=fixed.avail,
                            loc.prefix=paste(wd, "/Syrah/datFiles/", sep=""),
                            read.outs=TRUE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=temp_sigmaCat, temp_sigmaEsc=temp_sigmaEsc, wd=wd) # Usually cat=0.5, esc=0.1

  #Clear output objects
  cleanup(side=side, year=year, wd=wd)
  #Run ADMB 1st Round                          
  setwd(file.path(wd, "Syrah"))

  #With R2admb
  # run_admb("syrah", extra.args=paste("-ind datFiles/WestSide_",year,".dat -rs -nox", sep=''), verbose=TRUE)
  run_admb(model.name, extra.args=paste("-ind datFiles/WestSide_",year,".dat -rs -nox", sep=''), verbose=TRUE)
  
  move(side=side, year=year, wd=wd, model.name=model.name)

  #PLOT FINAL OUTPUT
  if(file.exists(file.path(wd, "Syrah", "outputFiles","WestSide","COR",paste0("WestSide_", year, ".cor")))) {
    

    plot.annual.catch.esc(side=side, years=plot.years, pdf=TRUE, text.cex=0.6, cross.cex=cross, wd=wd)
    plot.annual.agecomp(side=side, years=year, pdf=TRUE, input.cex=2, wd=wd)
    plot.annual.genComp(side, years=plot.years, pdf=TRUE, text.cex.left=1, text.cex.right=1, wd=wd)
    plot.maxGradient(side=side, years=plot.years, pdf=TRUE, wd=wd)
  }else {
    print(paste(side, 'SIDE RECONSTRUCTION DID NOT CONVERGE, RECHECK PHASING', sep=' '))	
  }
}

setwd(wd)

#######################################################################################################
#EAST SIDE
# phz.run=2; phz.sel=1; phz.avail=1; # 2015 req
# phz.run=2; phz.sel=2; phz.avail=1; # 2017 req
# phz.run=3; phz.sel=2; phz.avail=1; #2018 Best - sigma 0.5, 0.1 on first fit
# phz.run=3; phz.sel=2; phz.avail=1; #2019 Best - sigma 0.5, 0.05 on re-fit

phz.run=1; phz.sel=1; phz.avail=1;
temp_sigmaCat=0.5; temp_sigmaEsc=0.05 # For Second Fit

do.east <- TRUE
if(do.east==TRUE) {
  side <- 'east'
  
  # Reset Working Directory
  setwd(wd)
  
  #Cleanup
  cleanup(side=side, year=year, wd=wd)

  #Create ADMB Input
  if(gen.dat==TRUE) {
    fixed.avail=rep(-1,15) #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
  }else {
    #Get Average Availability
    east.prelim <- c(1964,1965,1983,1985,1993,1995,1999,2002,2006:(year-1))
    avg.avail <- avg.Avail.prelim(side='east', prelim.years=east.prelim, wd=wd)$avail.avgs
    
    fixed.avail=avg.avail  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
  }
  create.SYRAH.annual.input(side=side, district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                            stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                            # fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
                            # fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            fixed.avail=fixed.avail,
                            loc.prefix=paste(wd, "/Syrah/datFiles/", sep=""),
                            read.outs=FALSE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=0.5, temp_sigmaEsc=0.1, wd=wd)  #0.5, 0.5
  #Run ADMB 1st Round                          
  setwd(paste(wd, "/Syrah", sep=""))
  #system(paste("./syrah -ainp syrah.par -ind datFiles/EastSide_",year,".dat", sep=''))
  
  #With R2admb
  run_admb(model.name, extra.args=paste("-ind datFiles/EastSide_",year,".dat -rs -nox", sep=''), verbose=TRUE)
  
  move(side=side, year=year, wd=wd, model.name=model.name)
  
  #Plot
  # plot.all(plot.years=plot.years, plot.side=side, cross=cross, wd=wd)
  # plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross, wd=wd)
  # plot.maxGradient(side=side, years=plot.years, pdf=FALSE, wd=wd)
  
  #if(file.exists(paste("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=''))==FALSE) {
  #phz.run=1; phz.sel=2; phz.avail=1;

  #Create ADMB Input
  if(gen.dat==TRUE) {
    fixed.avail=rep(-1,15) #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
  }else {
    fixed.avail=avg.avail  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
  }
  create.SYRAH.annual.input(side=side, district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                            stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                            # fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (October/March Reconstruction)
                            # fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            fixed.avail=fixed.avail,
                            loc.prefix=paste(wd, "/Syrah/datFiles/", sep=""),
                            read.outs=TRUE, phz.run=phz.run, phz.sel=phz.sel, phz.avail=phz.avail,
                            temp_sigmaCat=temp_sigmaCat, temp_sigmaEsc=temp_sigmaEsc, wd=wd) #0.5, 0.05

  #Clear output objects
  cleanup(side=side, year=year, wd=wd)

  #Run ADMB 1st Round                          
  setwd(paste(wd, "/Syrah", sep=""))
  
  #With R2admb
  run_admb(model.name, extra.args=paste("-ind datFiles/EastSide_",year,".dat -rs -nox", sep=''), verbose=TRUE)
  
#   system(paste("./syrah -ainp syrah.par -ind datFiles/EastSide_",year,".dat", sep=''))
  
  move(side=side, year=year, wd=wd, model.name=model.name)
  
  #TEMPORARY
  # plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross, wd=wd)
  # plot.maxGradient(side=side, years=plot.years, pdf=FALSE, wd=wd)
  #Plot
  if(file.exists(file.path(wd, "Syrah", "outputFiles","EastSide","COR",paste0("EastSide_", year, ".cor")))) {
    # plot.all(plot.years=plot.years, plot.side=side, cross=cross, wd=wd)
    # setwd(paste(wd, "/Syrah/outputFiles", sep=""))
    plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross, wd=wd)
    plot.annual.agecomp(side=side, years=year, pdf=FALSE, input.cex=2, wd=wd)
    plot.annual.genComp(side, years=plot.years, pdf=FALSE, text.cex.left=0, text.cex.right=0, wd=wd)
    plot.maxGradient(side=side, years=plot.years, pdf=FALSE, wd=wd)
    
    
  }else {
    print(paste(side, 'SIDE RECONSTRUCTION DID NOT CONVERGE, RECHECK PHASING', sep=' '))	
  }
}

# plot.annual.catch.esc(side=side, years=plot.years, pdf=FALSE, text.cex=0.6, cross.cex=cross, wd=wd)

# plot.maxGradient(side=side, years=plot.years, pdf=FALSE, wd=wd)

# Reset Working Directory
setwd(wd)

#######################################################################################################
#CREATE BROOD TABLES - SOME SORT 
#  Must be done last as both converged outputs must be ready so offshore can be allocated accordingly
do.broods <- TRUE
if(do.broods==TRUE) {
  # west.converge <- file.exists(paste(wd, "/Syrah/outputFiles/WestSide/COR/WestSide_", year, ".cor", sep=""))
  #  east.converge <- file.exists(paste(wd, "/Syrah/outputFiles/EastSide/COR/EastSide_", year, ".cor", sep=""))
  # if(west.converge==TRUE & east.converge==TRUE) {
    create.all(plot.years=plot.years, plot.side='west', wd=wd)
    create.all(plot.years=plot.years, plot.side='east', wd=wd)
    plot.all(plot.years=plot.years, plot.side='west', cross=cross, wd=wd,pdf=TRUE)
    plot.all(plot.years=plot.years, plot.side='east', cross=cross, wd=wd,pdf=TRUE)
  # }else {
    # print(paste('ERROR: CANNOT CREATE TABLES, CONVERGENCE FOR WestSide: ', west.converge, ' CONVERGENCE FOR EastSide: ', east.converge, sep=''))
  # }
}







