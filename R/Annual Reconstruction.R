#******************************SS********************************************
#Project Name: SYRAH ANNUAL - Create input files
#Creator: Curry James Cunningham, SAFS, University of Washington
#Date: 8.15.11
#
#Purpose: Create input files for ADMB reconstruction
#**************************************************************************
#setwd("/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/R")
require(PBSmodelling)


#====================================== TESTING ================================================
# #Read in genetic, catch and escapement data
# gen.data <- na.omit(read.csv('GeneticsComp_updated_Annual.csv', header=TRUE, stringsAsFactors=FALSE))
# ac.data <- na.omit(read.csv('ageComp.annual.csv', header=TRUE, stringsAsFactors=FALSE)[,-1])
  
# catch.data <-  na.omit(read.csv('qry_Annual_CATCH.csv', header=TRUE, stringsAsFactors=FALSE))
# esc.data <- na.omit(read.csv('qry_Annual_ESCAPEMENT_updated.csv', header=TRUE, stringsAsFactors=FALSE))

# years <- c(1963:2011)
# n.years <- length(years)
# total.return <- vector(length=n.years)

# dist <- 325

# y <- 1
# for(y in 1:n.years) {
  # year <- years[y]
  
 # total.return[y] <- catch.data$SumOfTotal[catch.data$DistrictID==dist & catch.data$Year==year] + sum(esc.data$SumOfTotal[esc.data$DistrictID==dist & esc.data$Year==year &(esc.data$Stream==100 | esc.data$Stream==300 | esc.data$Stream==700)])

# }#next y

# write.csv(data.frame(years, total.return), file='Sum Run from Data.csv')

#===============================================================================================

#=================================== genetic.frac.matrix =======================================
#Returns a matrix of observed genetic porportions, in one year.
#Rows are individual sampling periods
# column 1: District ID
#	column 2: Sample Size
#	column 3+: reporting group porportions (in order called in inputs section)
#	
#Inputs: data, districts (name of fishing districts sampled), districtIDs (representing the fishing districts), 
#	 include(a Y/N from the .csv indicating whether or not to include that result), year,
#	 and reporting groups for genetic compaison (note this is the order they will appear in the matrix).

genetic.frac.matrix <- function(gen.data, districts, districtIDs, include, year, rep.groups, wd=wd) {
  setwd(paste(wd, "/R", sep=""))
  n.districts <- length(districts)
  n.rep.groups <- length(rep.groups)
  #Output matrix
  gen.mat <- matrix(nrow=length(districts), ncol=(length(rep.groups)+2))
  
  #Genetic Data Presence
  gen.present <- vector(length=n.districts)
  
  d <- 1
  for(d in 1:n.districts) {
    #District
    dist <- districts[d]
    
    #District number
    gen.mat[d,1] <- d
    
    #Determine if genetics are available for that year
    if(nrow(gen.data[gen.data$Year == year & gen.data$Include == include & gen.data$District.Catch == dist,]) > 0) {   #YES
      #Record presence of genetic data for district
      gen.present[d] <- 1
      #Determine data to be used in creating matrix
      temp.data <- gen.data[gen.data$Year == year & gen.data$Include == include & gen.data$District.Catch == dist,]

      #Sample size
      gen.mat[d,2] <- temp.data$Sample.Size
      #Fill in genetic proportions data
      r <- 1
      for(r in 1:n.rep.groups) {
        temp.rep.group <- rep.groups[r]
        gen.mat[d,(2+r)] <- as.numeric(temp.data[which(names(temp.data) == temp.rep.group)])
      }#next r
    } else {  #NO
      #Record absence of genetic data for district
      gen.present[d] <- -1
      #Record place holder data
      gen.mat[d,2] <- -1  #No sample size
      r <- 1
      for(r in 1:n.rep.groups) {gen.mat[d,(2+r)] <- 0  }
    }
  }#next d
  
  output <- NULL
  output$gen.mat <- gen.mat
  output$gen.present <- gen.present
  return(output)    
}#end genetic.frac.matrix

#gen.data <- read.csv('GeneticsComp_updated_Annual.csv', header=TRUE, stringsAsFactors=FALSE)

#WestSide
#genetic.frac.matrix(gen.data=gen.data, districts=c('Nushagak'), districtIDs=c(1), include='Y', year=2011, 
#                     rep.groups=c('Igushik','Wood','Nushagak'))
#EastSide
#genetic.frac.matrix(gen.data=gen.data, districts=c('NaknekKvichak','Egegik','Ugashik'), districtIDs=c(1,2,3),
#                     include='Y', year=2011,
#                      rep.groups=c('Kvichak','Alagnak','Naknek','Egegik','Ugashik'))



#=========== agecomp.year ===================================
#Age composition for catch or escapement.
#  For CATCH: supply the district.codes and districtIDs
#
#  For ESC: supply the district.codes, stream.codes (repeating district.codes as necessary) and streamIDs
#
#  NOTE: For location/year combinations with missing data:
#          1) If missing year is prior to first year of data, then first year's data will be assumed
#          2) Otherwise age composition and sample size will be average of preceeding and following years with
#               available data. 
#
# The second element is a matrix with:
#      column 1: districtIDs or streamIDs
#      column 2: sample size
#      columns 3 to n+2: proportion of fish recorded in each age group.
#Inputs: 1) data from read.agecomp
#        2) year
#	       3) type ('catch' or 'esc')
#	       4) district.codes (in sequence with area.codes)
#        5) districIDs (1 = NaknekKvichak, 2 = Egegik, 3 = Ugashik or 1 = Nushagak)
#	       6) stream.codes (in sequence with area.codes)
#        7) streamIDs 
#        8) agecomp.codes


agecomp.year <- function(ac.data, year, type, district.codes=0, districtIDs=0, stream.codes=0, 
                           streamIDs=0, agecomp.codes, wd=wd) {
  setwd(paste(wd, "/R", sep=""))
  #Ensures type input is of real type
  if(type != 'catch' & type != 'esc') { print('##### ERROR IN TYPE SELECTION #####'); stop() } 
  
  n.districts <- length(district.codes)
  n.streams <- length(stream.codes)
  n.agecomps <- length(agecomp.codes)
  
  if(type == 'catch') {  #Catch Data
    agecomp.mat <- matrix(data=0, nrow=n.districts, ncol=(n.agecomps+2))
    d <- 1
    for(d in 1:n.districts) {
      dist <- district.codes[d]
      #Fill in district ID
      agecomp.mat[d,1] <- districtIDs[d]
      if(nrow(ac.data[ac.data$catch.esc == type & ac.data$district == dist & ac.data$year == year,]) > 0) { #Age Composition Data Available
        #Fill in sample size
        agecomp.mat[d,2] <- ac.data$n.sample[ac.data$catch.esc == type & ac.data$district == dist & 
                                               ac.data$year == year] 
        #Fill in age composition PROPORTIONS
        agecomp.mat[d,c(3:ncol(agecomp.mat))] <- as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                      ac.data$district == dist & 
                                                                      ac.data$year == year, c(8:ncol(ac.data))]/
                                                                      ac.data$n.fish[ac.data$catch.esc == type & 
                                                                      ac.data$district == dist & ac.data$year == year])
      }#end if age comp data available
      else{  #NO Age Composition Data Available 
        print(paste('##### NO CATCH AGE COMP AVAILABLE FOR DISTRICT', dist, 'IN', year, sep=' '))
        #Years with data 
        temp.years <- sort(unique(ac.data$year[ac.data$catch.esc == type & ac.data$district == dist])) 
        
        if(year < min(temp.years)) {
          #Fill in SAMPLE SIZE (bit of a fudge factor)
          agecomp.mat[d,2] <- 0#ac.data$n.sample[ac.data$catch.esc == type & ac.data$district == dist & ac.data$year == min(temp.years)]
          agecomp.mat[d,c(3:ncol(agecomp.mat))] <- as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                        ac.data$district == dist & 
                                                                        ac.data$year == min(temp.years), c(8:ncol(ac.data))]/
                                                                        ac.data$n.fish[ac.data$catch.esc == type & 
                                                                        ac.data$district == dist & ac.data$year == min(temp.years)])
          
        }
        else {
          #Fill in SAMPLE SIZE (bit of a fudge factor)
          agecomp.mat[d,2] <- 0# mean(c(ac.data$n.sample[ac.data$catch.esc == type & 
                                                        # ac.data$district == dist & 
                                                        # ac.data$year == temp.years[max(which(temp.years < year))]], 
                                     # ac.data$n.sample[ac.data$catch.esc == type & 
                                                        # ac.data$district == dist & 
                                                        # ac.data$year == temp.years[min(which(temp.years > year))]]))
          #Find average of age comp data surrounding year of MISSING data
          ac <- 1
          for(ac in 1:n.agecomps) {
          
            agecomp.mat[d,(2+ac)] <- mean(c(as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                 ac.data$district == dist & 
                                                                 ac.data$year == temp.years[max(which(temp.years < year))], (7+ac)]/
                                                                 ac.data$n.fish[ac.data$catch.esc == type & 
                                                                 ac.data$district == dist & 
                                                                 ac.data$year == temp.years[max(which(temp.years < year))]]),
                                              as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                   ac.data$district == dist & 
                                                                   ac.data$year == temp.years[min(which(temp.years > year))], (7+ac)]/
                                                                   ac.data$n.fish[ac.data$catch.esc == type & 
                                                                   ac.data$district == dist & 
                                                                   ac.data$year == temp.years[min(which(temp.years > year))]])))
            
          }#next ac  
        }#if first year      
      }#if ac avail
    }#next d
  }#catch  
  else {  #Escapement data
    agecomp.mat <- matrix(data=0, nrow=n.streams, ncol=(n.agecomps+2))
    
    s <- 1
    for(s in 1:n.streams) { print(s)
      #Fill in stream ID
      agecomp.mat[s,1] <- streamIDs[s]
      if(nrow(ac.data[ac.data$catch.esc == type & ac.data$district == district.codes[s] &  
                        ac.data$stream == stream.codes[s] & ac.data$year == year,]) > 0) { #Age Comp Available
        #Fill in sample size
        agecomp.mat[s,2] <- ac.data$n.sample[ac.data$catch.esc == type & ac.data$district == district.codes[s] &
                                               ac.data$stream == stream.codes[s] & ac.data$year == year]
      
        #Fill in age composition PROPORTIONS
        agecomp.mat[s,c(3:ncol(agecomp.mat))] <- as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                      ac.data$district == district.codes[s] &
                                                                      ac.data$stream == stream.codes[s] & 
                                                                      ac.data$year == year, c(8:ncol(ac.data))] /
                                                            ac.data$n.fish[ac.data$catch.esc == type &
                                                                             ac.data$district == district.codes[s] &
                                                                             ac.data$stream == stream.codes[s] &
                                                                             ac.data$year == year])
      }
      else { #NO Age Comp data available
        print(paste('##### NO ESCAPEMENT AGE COMP AVAILABLE FOR STREAM', stream.codes[s], 'OF DISTRICT', district.codes[s], 'IN', year, sep=' '))
        
        #Years with available data
        temp.years <- sort(unique(ac.data$year[ac.data$catch.esc == type & ac.data$district == district.codes[s] & 
                                                 ac.data$stream == stream.codes[s]])) 
        
        if(year < min(temp.years)) {
          #Fill in SAMPLE SIZE (bit of a fudge factor)
          agecomp.mat[s,2] <- 0# ac.data$n.sample[ac.data$catch.esc == type & ac.data$district == district.codes[s] & 
                                                 # ac.data$stream == stream.codes[s] & ac.data$year == min(temp.years)]
          
          agecomp.mat[s,c(3:ncol(agecomp.mat))] <- as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                      ac.data$district == district.codes[s] &
                                                                      ac.data$stream == stream.codes[s] &
                                                                      ac.data$year == min(temp.years), c(8:ncol(ac.data))]/
                                                                      ac.data$n.fish[ac.data$catch.esc == type & 
                                                                      ac.data$district == district.codes[s] &
                                                                      ac.data$stream == stream.codes[s] &
                                                                      ac.data$year == min(temp.years)])
          
        }
        else {
          #Fill in SAMPLE SIZE (bit of a fudge factor)
          agecomp.mat[s,2] <- 0# mean(c(ac.data$n.sample[ac.data$catch.esc == type & ac.data$district == district.codes[s] & 
                                                        # ac.data$stream == stream.codes[s] & 
                                                        # ac.data$year == temp.years[max(which(temp.years < year))]], 
                                     # ac.data$n.sample[ac.data$catch.esc == type & ac.data$district == district.codes[s] & 
                                                        # ac.data$stream == stream.codes[s] &
                                                        # ac.data$year == temp.years[min(which(temp.years > year))]]))
          #Find average of age comp data surrounding year of MISSING data
          ac <- 1
          for(ac in 1:n.agecomps) {
            
            agecomp.mat[s,(2+ac)] <- mean(c(as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                 ac.data$district == district.codes[s] & 
                                                                 ac.data$stream ==  stream.codes[s] &
                                                                 ac.data$year == temp.years[max(which(temp.years < year))], (7+ac)]/
                                                         ac.data$n.fish[ac.data$catch.esc == type & 
                                                                          ac.data$district == district.codes[s] & 
                                                                          ac.data$stream == stream.codes[s] & 
                                                                          ac.data$year == temp.years[max(which(temp.years < year))]]),
                                            as.numeric(ac.data[ac.data$catch.esc == type & 
                                                                 ac.data$district == district.codes[s] &
                                                                 ac.data$stream == stream.codes[s] &
                                                                 ac.data$year == temp.years[min(which(temp.years > year))], (7+ac)]/
                                                         ac.data$n.fish[ac.data$catch.esc == type & 
                                                                          ac.data$district == district.codes[s] &
                                                                          ac.data$stream ==  stream.codes[s] &
                                                                          ac.data$year == temp.years[min(which(temp.years > year))]])))  
            
          }#next ac
        }#first year
      }#ac data available
    }#next s 
  }#escapement
  colnames(agecomp.mat) <- c('ID','n.sample',agecomp.codes)
  return(agecomp.mat)
}
#west.prelim <- c(1980,1982,1983,1985,1993,1995,1999,2006:2011)
#  ac.data <- read.csv('ageComp.annual.csv', header=TRUE, stringsAsFactors=FALSE)[,-1]

#Catch
  #WestSide
  #agecomp.year(ac.data, year=1980, type='catch', district.codes=325, districtIDs=1, stream.codes=0, streamIDs=0, 
  #               agecomp.codes=c("0.1","0.2","0.3","0.4","0.5","1.1","1.2","1.3","1.4","1.5",
  #                                 "2.1","2.2","2.3","2.4","3.1","3.2","3.3","3.4"))
  #EastSide
  #agecomp.year(ac.data, year=2008, type='catch', district.codes=c(324,322,321), districtIDs=c(1,2,3), 
  #               stream.codes=0, streamIDs=0, agecomp.codes=c("0.1","0.2","0.3","0.4","0.5","1.1","1.2",
  #                                                              "1.3","1.4","1.5","2.1","2.2","2.3","2.4","3.1",
  #                                                              "3.2","3.3","3.4"))
#Escapement
  #WestSide
  #agecomp.year(ac.data, year=2011, type='esc', district.codes=c(325,325,325), districtIDs=0,
  #               stream.codes=c(100,300,700),
  #              streamIDs=c(1,2,3), agecomp.codes=c("0.1","0.2","0.3","0.4","0.5","1.1","1.2",
  #                                                     "1.3","1.4","1.5","2.1","2.2","2.3","2.4","3.1",
  #                                                    "3.2","3.3","3.4"))
  #EastSide
#  agecomp.year(ac.data, year=2001, type='esc', district.codes=c(324,324,324,322,321), districtIDs=0,
#                 stream.codes=c(100,500,600,100,100),
#                 streamIDs=c(1,2,3,4,5), agecomp.codes=c("0.1","0.2","0.3","0.4","0.5","1.1","1.2",
#                                                       "1.3","1.4","1.5","2.1","2.2","2.3","2.4","3.1",
#                                                       "3.2","3.3","3.4"))



#==========================================================================
#Function to create annual input files for EastSide or WestSide
#
# INPUTS:  1) side = 'west' or 'east'
#          2) district.codes - district reference codes (repeated as necessary for eastside)
#          3) stream.codes - stream reference codes
#          4) stream.district - district to which a stream belongs (length will be length of n.stocks, but will have repeats for east)
#          5) year - year for which .dat file is being generated
#          6) cat.esc.div - divisor for catch and escapement (1 none, 1000 all measurements in units of 1000 fish)
#          7) fixed.avail - fixed availability values if 
#          8) loc.prefix - prefix for sending files to another location
#          9) read.outs - whether or not to read RunSize(s) from previous outputfiles
#         10) phz.run - estimation phase for RunSize
#         11) phz.sel - est. phase for selectivity parameters
#         12) phz.avail - est. phase for availability parameters
#         13) temp_sigmaCat - standard deviation for lognormal obs err. distribution on CATCH
#         14) temp_sigmaCat - standard deviation for lognormal obs err. distribution on CATCH
#
#==========================================================================
create.SYRAH.annual.input <- function(side='west', district.codes, stream.codes, stream.district, year, cat.esc.div=1, fixed.avail, loc.prefix=NULL, 
                                        read.outs=FALSE, phz.run=1, phz.sel=1, phz.avail=1, temp_sigmaCat=0.5, temp_sigmaEsc=0.5, wd=wd) {
  setwd(paste(wd, "/R", sep=""))
  ########### Testing ##############
   # side <- 'west'
   # district.codes <- c(325,325,325)
   # stream.codes <- c(100,300,700)
   # stream.district <- c(1,1,1)
   # year <- 1994
   # outputfile <- 'test.dat'
   # fixed.avail <- rep(-1,3)
   # loc.prefix <- NULL
   # cat.esc.div=1
  ##################################
   #   side <- 'east'
  #    district.codes <- c(324,324,324,322,321)
   #   stream.codes <- c(100,500,600,100,100)
    #  stream.district <- c(1,1,1,2,3)
    #  year <- 2025
    #  outputfile <- 'test.dat'
     # fixed.avail <- 16:30
     #loc.prefix=NULL
     # cat.esc.div <- 1
     # read.outs <- FALSE
     # phz.run <- 1
     # phz.sel <- 1
     # phz.avail <- 1
  ##################################
  
  #Read in genetic, catch and escapement data
  gen.data <- na.omit(read.csv('GeneticsComp_updated_Annual.csv', header=TRUE, stringsAsFactors=FALSE))
  ac.data <- na.omit(read.csv('ageComp.annual.csv', header=TRUE, stringsAsFactors=FALSE)[,-1])
  
  catch.data <-  na.omit(read.csv('qry_Annual_CATCH_updated.csv', header=TRUE, stringsAsFactors=FALSE))
  esc.data <- na.omit(read.csv('qry_Annual_ESCAPEMENT_updated.csv', header=TRUE, stringsAsFactors=FALSE))
  ### For Divisor ###
  catch.data$SumOfTotal <- catch.data$SumOfTotal/cat.esc.div
  esc.data$SumOfTotal <- esc.data$SumOfTotal/cat.esc.div
  
  
  agecomp.codes <- c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4")
  agecompIDs <- c(c(1:5), c(1:5), c(1:4), c(1:4))  #Ocean year reference IDs for agecomps

  #Ensure the side is correct
  if(side != 'west' & side != 'east') { print('##### ERROR IN SIDE SELECTION #####'); stop() }
  

  
  if(side == 'west') { 
    n.districts <- 1 
    outputfile <- paste(loc.prefix,'WestSide_', year, '.dat', sep='')
  }else { 
    n.districts <- 3
    outputfile <- paste(loc.prefix,'EastSide_', year, '.dat', sep='')
  }
  n.agecomps <- length(agecomp.codes)
  n.stocks <- length(stream.codes)
  n.groups <- n.stocks*n.agecomps
  n.avail.par <- n.stocks*n.districts
  
  
  
  write(x="#SYRAH Annual input file, version number:", file=outputfile, append=FALSE)
  if(side == 'west') { write(x="1.0", file=outputfile, append=TRUE) } else { write(x="2.0", file=outputfile, append=TRUE) }
  write(x=paste('#DATA is for the', side, 'side in', year, sep=' '), file=outputfile, append=TRUE)
  write(x='#(year)', file=outputfile, append=TRUE)
  write(x=year, file=outputfile, append=TRUE)
  write(x="#CONSTANTS THAT DETERMINE ARRAY BOUNDS", file = outputfile, append=TRUE)
  write(x="#Number of districts", file = outputfile, append=TRUE)
  write(x="#(NDISTRICTS)", file = outputfile, append=TRUE)
  write(x = n.districts, file = outputfile, append=TRUE)
  write(x="#Number of groups", file = outputfile, append=TRUE)
  write(x="#(NGROUPS)",file = outputfile, append=TRUE)
  write(x = n.groups, file = outputfile, append=TRUE)
  write(x='#Number of stocks', file=outputfile, append=TRUE)
  write(x='#(NSTOCKS)', file=outputfile, append=TRUE)
  write(x=n.stocks, file=outputfile, append=TRUE)
  write(x="#Number of age groups recorded: ", file = outputfile, append=TRUE)
  write(x="#(NAGECOMPS)", file = outputfile, append=TRUE)
  write(x = n.agecomps, file = outputfile, append=TRUE)
  write(x="#Age group labels (must be numeric!)", file = outputfile, append=TRUE)
  write(x="#(AgeCompLabels)", file = outputfile, append=TRUE)
  write(x=agecomp.codes, file = outputfile, append=TRUE, ncolumns=n.agecomps)

  #GENETIC COMPOSITION
  if(side == 'west') {  #WestSide
    #Retreive Data
    temp.gen <- genetic.frac.matrix(gen.data=gen.data, districts=c('Nushagak'), districtIDs=c(1), include='Y', 
                                             year=year, rep.groups=c('Igushik','Wood','Nushagak'), wd=wd)
      
      
    write(x="#Flag for genetic data by district: 1 = Data available, -1 = Data Unavailable ", file = outputfile, append=TRUE)
    write(x="#(GENdata)", file = outputfile, append=TRUE)
    write(x=temp.gen$gen.present, file = outputfile, append=TRUE)
    write(x="#Genetic fractions", file = outputfile, append=TRUE)
    write(x="#(GeneticFracData)", file = outputfile, append=TRUE)
    write(x='#1) District ID, 2) Sample Size, 3) Igushik, 4) Wood, 5) Nushagak', file=outputfile, append=TRUE)
    write(x=t(temp.gen$gen.mat), file=outputfile, ncolumns=ncol(temp.gen$gen.mat), append=TRUE, sep='     ')
  }else {  #EastSide
    #Retreive Data
    temp.gen <- genetic.frac.matrix(gen.data=gen.data, districts=c('NaknekKvichak','Egegik','Ugashik'), 
                                           districtIDs=c(1,2,3), include='Y', year=year, 
                                           rep.groups=c('Kvichak','Alagnak','Naknek','Egegik','Ugashik'), wd=wd)
    
    write(x="#Flag for genetic data by district: 1 = Data available, -1 = Data Unavailable ", file = outputfile, append=TRUE)
    write(x="#(GENdata)", file = outputfile, append=TRUE)
    write(x=temp.gen$gen.present, file = outputfile, append=TRUE)
    write(x="#Genetic fractions", file = outputfile, append=TRUE)
    write(x="#(GeneticFracData)", file = outputfile, append=TRUE)
    write(x='#1) District ID, 2) Sample Size, 3) Kvichank, 4) Alagnak, 5) Naknek, 6) Egegik, 7) Ugashik', file=outputfile, append=TRUE)  
    write(x=t(temp.gen$gen.mat), file=outputfile, ncolumns=ncol(temp.gen$gen.mat), append=TRUE, sep='     ')
  }
  #AGE COMPOSITION
  write(x='#Age composition for CATCH data', file=outputfile, append=TRUE)
  write(x='#(AgeCompCATCH)', file=outputfile, append=TRUE)
  if(side == 'west') {
    temp.ac.cat <- agecomp.year(ac.data=ac.data, year=year, type='catch', district.codes=c(325), districtIDs=c(1),
                                  stream.codes=0, streamIDs=0, agecomp.codes=agecomp.codes, wd=wd) 
    write(x=t(temp.ac.cat), file=outputfile, ncolumns=ncol(temp.ac.cat), append=TRUE, sep='     ')
  }else {
    temp.ac.cat <- agecomp.year(ac.data=ac.data, year=year, type='catch', district.codes=c(324,322,321),
                                  districtIDs=c(1,2,3), stream.codes=0, streamIDs=0, agecomp.codes=agecomp.codes, wd=wd)    
    write(x=t(temp.ac.cat), file=outputfile, ncolumns=ncol(temp.ac.cat), append=TRUE, sep='     ')
  }
  write(x='#Age composition for ESCAPEMENT data', file=outputfile, append=TRUE)
  write(x='#(AgeCompESC)', file=outputfile, append=TRUE)
  if(side == 'west') {
    temp.ac.esc <- agecomp.year(ac.data=ac.data, year=year, type='esc', district.codes=c(325,325,325), districtIDs=0,
                                  stream.codes=c(100,300,700), streamIDs=c(1,2,3), agecomp.codes=agecomp.codes, wd=wd)
    write(x=t(temp.ac.esc), file=outputfile, ncolumns=ncol(temp.ac.esc), append=TRUE, sep='     ')
  }else {
    temp.ac.esc <- agecomp.year(ac.data=ac.data, year=year, type='esc', district.codes=c(324,324,324,322,321), districtIDs=0,
                                  stream.codes=c(100,500,600,100,100), streamIDs=c(1,2,3,4,5), agecomp.codes=agecomp.codes, wd=wd)
    write(x=t(temp.ac.esc), file=outputfile, ncolumns=ncol(temp.ac.esc), append=TRUE, sep='     ')
  }
  write(x='#Catch Data by District', file=outputfile, append=TRUE)
  write(x='#(catchData)', file=outputfile, append=TRUE)
  if(side == 'west') {  ######### PROBLEM HERE !!!!!!!!!! - FIXED
    temp.catch <- as.numeric(c(catch.data$SumOfTotal[catch.data$Year == year & catch.data$DistrictID == 325]))  #Nushagak
    write(x=t(temp.catch), file=outputfile, ncolumns=length(temp.catch), append=TRUE, sep='     ')
  }else {
    temp.catch <- as.numeric(c(catch.data$SumOfTotal[catch.data$Year == year & catch.data$DistrictID == 324], #NaknekKvi
                                 catch.data$SumOfTotal[catch.data$Year == year & catch.data$DistrictID == 322],  #Egegik
                                 catch.data$SumOfTotal[catch.data$Year == year & catch.data$DistrictID == 321]))  #Ugashik  
    write(x=t(temp.catch), file=outputfile, ncolumns=length(temp.catch), append=TRUE, sep='     ')   
  }
  write(x='#Escapement Data by Stock', file=outputfile, append=TRUE)
  write(x='#(escData)', file=outputfile, append=TRUE)
  temp.esc <- vector(length=n.stocks)
  s <- 1
  for(s in 1:n.stocks) {  #ERROR HERE -returning a vector of c(NA, true value) - solved with na.omit() on read in data frames
    temp.esc[s] <- as.numeric(esc.data$SumOfTotal[esc.data$Year == year & esc.data$DistrictID == district.codes[s]  & 
                                         esc.data$Stream == stream.codes[s]])  
  }#next s  
  write(x=t(temp.esc), file=outputfile, ncolumns=length(temp.esc), append=TRUE, sep='     ')
  
  #===== PARAMETERS TO ESTIMATE =====
  write(x="#PARAMETERS TO BE ESTIMATED", file = outputfile, append=TRUE)  
  #SELECTIVITY - will need to be changed if we want to structure age groups differently.
  write(x='#Number of Selectivity Parameters', file=outputfile, append=TRUE)
  write(x='#(NSELECTPAR)', file=outputfile, append=TRUE)
  write(x=5, file=outputfile, append=TRUE)
  write(x='#Selectivity to estimate for AGE GROUPS NOT GROUPS age groups are 1.2, 1.3, 2.2, 2.3 ect...', file=outputfile, append=TRUE)
  write(x='#1) Age Group, 2) phase, 3) StartValue, 4) min, 5) max', file=outputfile, append=TRUE)
  write(x='#(TempSelectivity)', file=outputfile, append=TRUE)
  
  if(read.outs == TRUE) {
    if(side == 'west') {
      temp.data <- readList(paste(wd, "/Syrah/outputFiles/WestSide/WestSide_",year,".out",sep=""))
    } else {
      temp.data <- readList(paste(wd, "/Syrah/outputFiles/EastSide/EastSide_",year,".out",sep=""))  
    }    
    #Write parameters
    s <- 1
    for(s in 1:5) {
      write(x=paste(s, phz.sel, temp.data$selectivity[s]+0.1,'0.0     10', sep='     '), file=outputfile, append=TRUE)
    }#next s
  } else {
    write(x=paste('1', phz.sel, '0.25      0.0     10', sep='     '), file=outputfile, append=TRUE)
    write(x=paste('2', phz.sel, '0.75      0.0     10', sep='     '), file=outputfile, append=TRUE)
    write(x=paste('3', phz.sel, '1.00      0.0     10', sep='     '), file=outputfile, append=TRUE)
    write(x=paste('4', phz.sel, '1.25      0.0     10', sep='     '), file=outputfile, append=TRUE)
    write(x=paste('5', phz.sel, '1.50      0.0     10', sep='     '), file=outputfile, append=TRUE)
  }

  #AVAILABILITY
  write(x='#Number of Availability Parameters', file=outputfile, append=TRUE)
  write(x='#(NAVAILPAR)', file=outputfile, append=TRUE)
  write(x=n.avail.par, file=outputfile, append=TRUE)
  write(x='#Availability to estimate for stocks in districts (i.e. Alagnak in Egegik District)', file=outputfile, append=TRUE)
  write(x='#1) Availability group, 2) phase, 3) StartValue, 4) min, 5) max', file=outputfile, append=TRUE)
  write(x='#(TempAvailability)', file=outputfile, append=TRUE)
  
  if(side == 'west') {
    init.values <- c(0.25,1.75,1.0)
  } else {
    init.values <- c(1.5, 0.2, 0.02,
                     1.5, 0.2, 0.02,
                     1.5, 0.2, 0.02,
                     0.2, 4.0, 0.3,
                     0.2, 0.3, 4.0)
  }
  #Reading output
  if(read.outs == TRUE) {
    if(side == 'west') {
      temp.data <- readList(paste(wd, "/Syrah/outputFiles/WestSide/WestSide_", year, ".out", sep=""))
    } else {
      temp.data <- readList(paste(wd, "/Syrah/outputFiles/EastSide/EastSide_", year, ".out", sep=""))	
    }
  }
  #Writing availability_temp
  counter <- 1
  s <- 1
  for(s in 1:n.stocks) {
    d <- 1
    for(d in 1:n.districts) {
      if(temp.gen$gen.present[d] == 1) {  #GEN DATA AVAILABLE
        if(read.outs == FALSE) {
          write(x=paste(counter, phz.avail,  init.values[counter], '0.0     10', sep='     '), file=outputfile, append=TRUE)
        } else {
          write(x=paste(counter, phz.avail,  temp.data$availabilityVector[counter]+0.1, '0.0     10', sep='     '), file=outputfile, append=TRUE)	
        }
      } else {
        write(x=paste(counter, -1, fixed.avail[counter], 0.0, 10, sep='     '), file=outputfile, append=TRUE) 
      }
      #Update counter
      counter <- counter+1
    }#next d
  }#next s
   
  write(x='#Total run size of each group (thousands): group number, estimation phase [neg=ignore], startValue, min, max', 
          file=outputfile, append=TRUE)
  write(x='#(TempRunSize)', file=outputfile, append=TRUE)
  total.return <- sum(temp.catch) + sum(temp.esc)

  if(read.outs == TRUE) { #GETTING MORE PHZ.RUN == -1'S 
    temp.runSize <- temp.data$RunSize
    
    group.counter <- 1  #Temporary counter for the group number
    s <- 1
    for(s in 1:n.stocks) {
      ac <- 1
      for(ac in 1:n.agecomps) {
        if(temp.ac.esc[s,(ac+2)] + sum(temp.ac.cat[,(ac+2)]) == 0) {  #No fish of this age are in the escapement, therefore fix RunSize at 0 ###########ERROR HERE!
          write(x=paste(group.counter, -1, 0, 0, 0, sep='     '), file=outputfile, append=TRUE) 	
        } else {
          write(x=paste(group.counter, phz.run, temp.runSize[group.counter]+1,ifelse(temp.runSize[group.counter]<1,0,0.2*temp.runSize[group.counter]+1), 5*ifelse(temp.runSize[group.counter]<100,1000,temp.runSize[group.counter])+1, sep='     '), file=outputfile, append=TRUE)	
        }
        group.counter <- group.counter+1
      }#next ac
    }#next s
  } else { #GETTING FEWER PHZ.RUN == -1

    group.counter <- 1
    s <- 1
    for(s in 1:n.stocks) {
      #print(paste('s', s, sep=' '))
      ac <- 1
      for(ac in 1:n.agecomps) {  #Updated to ensure that if 0% observed in ESC for an AC, then that RunSize is fixed at zero
        #print(paste('ac', ac, sep=' '))
        #temp.start <- round((total.return/n.stocks)*temp.ac.esc[s,(ac+2)])
        temp.start <- round((total.return*(temp.esc[s]/sum(temp.esc)))*temp.ac.esc[s,(ac+2)])
        #ERROR HERE!
        #print(paste('temp.ac.esc', temp.ac.esc, sep=' '))
        #print(paste('temp.ac.esc[s,(ac+2)]',temp.ac.esc[s,(ac+2)],sep=' '))
        #print(paste('sum(temp.ac.cat[,(ac+2)])',sum(temp.ac.cat[,(ac+2)]),sep=' '))	
      	if(temp.ac.esc[s,(ac+2)] + sum(temp.ac.cat[,(ac+2)]) == 0) {
      	  write(x=paste(group.counter, -1, 0, 0, 0, sep='     '), file=outputfile, append=TRUE)
      	} else {
      	  write(x=paste(group.counter, phz.run, temp.start+1, ifelse(temp.start<1,0,0.2*temp.start+1), 5*ifelse(temp.start<100,1000,temp.start)+1, sep='     '), file=outputfile, append=TRUE)
      	}
      	group.counter <- group.counter+1
      }
    }#next s
  }
  
  write(x="#GROUP CODES", file = outputfile, append=TRUE)
  write(x="#These relate the groups to the various parameters of interest that could vary or be fixed", 
          file=outputfile, append=TRUE)
  write(x='#1) Group number, 2) DistrictID, 3) StreamID, 4) Selectivity pointer, 5) AgeComp group ID', file=outputfile, append=TRUE)
  write(x='#(groupCodes)', file=outputfile, append=TRUE)
  
  #Create GroupCodes matrix and write to file.
  groupCodes.mat <- matrix(data=0, nrow=n.groups, ncol=5)
  counter <- 1
    
  s <- 1
  for(s in 1:n.stocks) {
    ac <- 1
    for(ac in 1:n.agecomps) {
      groupCodes.mat[counter,1] <- counter  #Group number
      groupCodes.mat[counter,2] <- stream.district[s]  #DistrictID
      groupCodes.mat[counter,3] <- s  #StreamID
      groupCodes.mat[counter,4] <- agecompIDs[ac]  #Selectivity pointer
      groupCodes.mat[counter,5] <- ac  #AgeComp group ID
      counter <- counter+1
    }#next ac
  }#next s
  write(x=t(groupCodes.mat), file=outputfile, ncolumns=ncol(groupCodes.mat), append=TRUE, sep='     ')  
  #Variance Values to be specified
  write(x='#Standard deviation for CATCH error distribution', file=outputfile, append=TRUE)
  write(x='#(temp_sigmaCat)', file=outputfile, append=TRUE)
  write(x=temp_sigmaCat, file=outputfile, append=TRUE)
  write(x='#Standard deviation for ESCAPEMENT error distribution', file=outputfile, append=TRUE)
  write(x='#(temp_sigmaEsc)', file=outputfile, append=TRUE)
  write(x=temp_sigmaEsc, file=outputfile, append=TRUE)
  
  write(x='#Testing code to ensure data was read in correctly, DO NOT CHANGE', file=outputfile, append=TRUE)
  write(x='#(testCode)', file=outputfile, append=TRUE)
  write(x=12345, file=outputfile, append=TRUE)
}

#==========================================================================
#Find average availability values for use in years without genetic compostion of catch data.
#
# INPUTS:  1) side = 'west' or 'east'
#          2) prelim.years - preliminary years from which to draw values
#
# OUTPUTS: A vector of average availability values
#==========================================================================
avg.Avail.prelim <- function(side, prelim.years, wd=wd) {
  setwd(paste(wd, "/R", sep=""))
  #### TESTING ####
  #side <- 'west'
  #prelim.years <- 2006:2011
  ################
  if(side != 'west' & side != 'east') { print('##### ERROR: SIDE SELECTION INCORRECT!'); stop() }
  if(side == 'west') {
    n.avail.param <- 3
  } else {
    n.avail.param <- 15
  }
  n.years <- length(prelim.years)
  avail.values <- matrix(nrow=n.years, ncol=n.avail.param)
  
  y <- 1
  for(y in 1:n.years) {
    year <- prelim.years[y]
    if(side == 'west') {
      temp.data <- readList(paste(wd, "/Syrah/outputFiles/WestSide/WestSide_", year, '.out', sep="")) 
    }else {
      temp.data <- readList(paste(wd, "/Syrah/outputFiles/EastSide/EastSide_", year, '.out', sep=""))	
    }
    #Add to data matrix
    avail.values[y,] <- temp.data$availability
  }#next y
  output <- NULL
  output$avail.values <- avail.values
  output$avail.avgs <- apply(avail.values, 2, mean)
  output$avail.medians <- apply(avail.values, 2, median)
  output$avail.sd <- apply(avail.values, 2, sd)
  return(output)
}
#avg.Avail.prelim(side='east', prelim.years=c(2006:2011))$avail.avgs[15]



#====================================================================================================
#================= CREATE INPUT FILES COMMAND ==============================
  # ########### Testing ##############
  # side <- 'west'
  # district.codes <- c(325,325,325)
  # stream.codes <- c(100,300,700)
  # stream.district <- c(1,1,1)
  # year <- 2008
  # outputfile <- 'test.dat'
  # genetics <- TRUE
  # ##################################
  # side <- 'east'
  # district.codes <- c(324,324,324,322,321)
  # stream.codes <- c(100,500,600,100,100)
  # stream.district <- c(1,1,1,2,3)
  # year <- 2008
  # outputfile <- 'test.dat'
  # genetics <- TRUE
  # ##################################

                            


#Create input for recent years
##### WestSide #####
#All years with genetic (some missing age comp)
#west.allgen <- c(1965,1977,1980,1982,1983,1985,1993,1995,1999,2006:2011)#1,1,1
#west.allgen.nonCon <- c(1965,1985)


# #All Years with genetics
# for(year in west.allgen.nonCon) {
  # print(year)
  # create.SYRAH.annual.input(side='west', district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                            # stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                            # fixed.avail=rep(-1,3), loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            # read.outs=TRUE, phz.run=1, phz.sel=1, phz.avail=1)
# }#next year

#Create average availability values (using only years with BOTH age comp and genetics)
#All years with genetic and age comp (used for average Availability values)
#west.prelim <- c(1980,1982,1983,1985,1993,1995,1999,2006:2011)
#########################################################################
# west.prelim <- c(1965,1977,1980,1982,1983,1985,1993,1995,1999,2006:2011)
# avg.avail <- avg.Avail.prelim(side='west', prelim.years=west.prelim)$avail.avgs
#########################################################################


#Main reconstruction
#west.main <- c(1963:1964,1966:1976,1978:1979,1981,1984,1986:1992,1994,1996:1998,2000:2005)
#west.main.nonCon <- c(1971,1975,1976,1978,1981,1985,1987,2002)
#west.main.nonCon2 <- c(1978,1981,1987)
#west.main.nonCon3 <- c(2011)
#west.new <- 2013

#MAIN RUNS
#for(year in west.new) {
#  print(year)
#  create.SYRAH.annual.input(side='west', district.codes=c(325,325,325), stream.codes=c(100,300,700), 
#                            stream.district=c(1,1,1), year=year, cat.esc.div=1, 
#                            #fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
#                            fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
#                            loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
#                            read.outs=TRUE, phz.run=1, phz.sel=2, phz.avail=2,
#                            temp_sigmaCat=) 
#}

#########################################################################
#For Additional Years
# year <- 2013
# create.SYRAH.annual.input(side='west', district.codes=c(325,325,325), stream.codes=c(100,300,700), 
                          # stream.district=c(1,1,1), year=year, cat.esc.div=1, 
                          # fixed.avail=avg.avail, #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                          # #fixed.avail=rep(-1,3), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                          # loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                          # read.outs=FALSE, phz.run=1, phz.sel=2, phz.avail=2,
                          # temp_sigmaCat=0.5, temp_sigmaEsc=0.5) 

#########################################################################



# ##### EastSide #####
# #All years with genetic (some missing age comp)
# east.allgen <- c(1964,1965,1983,1985,1993,1995,1999,2002,2006:2011)#0.5, 0.1, (1,2,2)
# east.allgen.nonCon <- c(1964,1983,1985,1999,2002,2008,2009,2011)
# east.allgen.nonCon2 <- c(1964,1983,1985,2009,2010)
# east.allgen.nonCon3 <- c(1964,1983,1985,2010)
# east.allgen.nonCon4 <- c(1964,1983,2010)
# east.allgen.nonCon5 <- 1964

# #All Years with genetics
# for(year in east.allgen.nonCon5) {
  # print(year)
  # create.SYRAH.annual.input(side='east', district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                            # stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                            # fixed.avail=rep(-1,15), loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            # read.outs=FALSE, phz.run=2, phz.sel=1, phz.avail=1)
# }#next year                     

# #Create average availability values (using only years with BOTH age comp and genetics)
# #All years with genetic and age comp (used for average Availability values)
# #east.prelim <- c(1965,1999,2006:2011)
#########################################################################

# east.prelim <- c(1964,1965,1983,1985,1993,1995,1999,2002,2006:2011)
# avg.avail <- avg.Avail.prelim(side='east', prelim.years=east.prelim)$avail.avgs

#########################################################################

# #MAIN RUNS                            
# #Main reconstruction
# east.main <- c(1963,1966:1982,1984,1986:1992,1994,1996:1998,2000:2001,2003:2005)
# east.main.nonCon <- c(1971,1972,1976,1977,1980,1987,1989,1997,2001)
# east.main.nonCon2 <- c(1971,1972,1976,1977,1980,1989)
# east.main.nonCon3 <- c(1971,1976,1980,1989,1997)
# east.main.nonCon4 <- c(1971,1980,1997)
# east.main.nonCon5 <- c(1991,2004)
#########################################################################

# east.new <- 2013



# for(year in east.new) {
  # print(year)
  # create.SYRAH.annual.input(side='east', district.codes=c(324,324,324,322,321), stream.codes=c(100,500,600,100,100), 
                            # stream.district=c(1,1,1,2,3), year=year,  cat.esc.div=1,
                            # #fixed.avail=avg.avail,  #IF NO GENETIC DATA ARE AVAILABLE (March Reconstruction)
                            # fixed.avail=rep(-1,15), #IF GENETIC DATA ARE AVAILABLE (June Reconstruction)
                            # loc.prefix="/Users/curryc2/Documents/Curry's SYRAH Work/Syrah Annual/Syrah/datFiles/",
                            # read.outs=TRUE, phz.run=1, phz.sel=1, phz.avail=1)
# }#next year                           
#########################################################################
                            