#!/usr/bin/env Rscript
# Clean-up
rm(list = ls())

# List of packages
packages <- c("argparse", "ncdf4")

# Check if packages exist. If not, install them
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

suppressPackageStartupMessages(library("argparse"))

# create parser object
parser <- ArgumentParser()
parser$add_argument("-v", "--verbose", action="store_true",
                    help="Print extra output")
parser$add_argument("-i", "--input", 
                    help="Input mask file. This file should be a NetCDF file with 'lat' and 'lon' dimensions and variables.")
parser$add_argument("-o", "--output", 
                    help="Output domain file for VIC.This is a NetCDF file")
parser$add_argument("-s", "--sellonlatbox", nargs=4, type="double",
                    help="Select lon/latbox. Eg. -s -359.75 359.75 -89.75 89.75")
# parser$add_argument("-c", "--count", type="integer", default=5, 
#                     help="Number of random normals to generate [default %(default)s]",
#                     metavar="number")
# parser$add_argument("--generator", default="rnorm", 
#                     help = "Function to generate random deviates [default \"%(default)s\"]")
# parser$add_argument("--mean", default=0, type="double",
#                     help="Mean if generator == \"rnorm\" [default %(default)s]")
# parser$add_argument("--sd", default=1, type="double",
#                     metavar="standard deviation",
#                     help="Standard deviation if generator == \"rnorm\" [default %(default)s]")

## Convert VIC things

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
parser$print_usage()


write("Create domain file...", stdout())
# print some progress messages to stderr
if ( args$verbose ) { 
  write("writing some verbose output to standard error...\n", stderr()) 
}

get_script_arguments <- function(){
  args$input <- "./Precip_1day.nc4"
  args$output <- "./domain.nc4"
  args$sellonlatbox <- c(-179.75, 179.75, -89.75, 89.75)
  args$sellonlatbox <- c(-24.25, 39.75, 33.25, 71.75)
  args$sellonlatbox <- c(5.25, 7.25, 51.25, 52.75)
  setwd("/media/psf/Home/Desktop/WERK/VIC_params/vic_convert")
  return(args)
}

generate <- function(){
  # do some operations based on user input
  if( args$generator == "rnorm") {
    cat(paste(rnorm(args$count, mean=args$mean, sd=args$sd), collapse="\n"))
  } else {
    cat(paste(do.call(args$generator, list(args$count)), collapse="\n"))
  }
  cat("\n")
}

#args<-get_script_arguments() # enable this line if running from script and not from commandline
write(paste0("Current directory: ", getwd()), stdout())

write(paste0("File: ",args$input), stdout())

lonlatbox<-args$sellonlatbox
print(lonlatbox)


suppressPackageStartupMessages(library("ncdf4"))



# READ FROM NETCDF
ncin <- nc_open( filename = args$input)
ncInLon <- ncvar_get(ncin, varid = "lon")
ncInLat <- ncvar_get(ncin, varid = "lat")
ncInData <- ncvar_get(ncin)
ncInData[!is.na(ncInData)] <- 1
#ncInData[is.na(ncInData)] <- 0

ncMask<-ncInData[which(ncInLon==lonlatbox[1]):which(ncInLon==lonlatbox[2]),which(ncInLat==lonlatbox[3]):which(ncInLat==lonlatbox[4])]












# WRITE TO NETCDF
resolution=0.5
lons<-seq(args$sellonlatbox[1],args$sellonlatbox[2],resolution)
lats<-seq(args$sellonlatbox[3],args$sellonlatbox[4],resolution)

xcs<-matrix(lons, ncol=length(lats), nrow=length(lons), byrow = FALSE)
ycs<-matrix(lats, ncol=length(lats), nrow=length(lons), byrow = TRUE)

areas<-matrix(ncol=length(lats), nrow=length(lons), data = 1)
fracs<-matrix(ncol=length(lats), nrow=length(lons), data = 1)
masks<-matrix(ncol=length(lats), nrow=length(lons), data = 1)
run_cellss<-matrix(ncol=length(lats), nrow=length(lons), data = 1)

if (length(args$input) > 0) {
  write("NetCDF mask applied!\n",stdout())

  # Makout data
  areas[is.na(ncMask)] <- 0
  fracs[is.na(ncMask)] <- 0
  masks[is.na(ncMask)] <- 0
  run_cellss[is.na(ncMask)] <- 0
}
# Write to NetCDF
dimLon <- ncdim_def(name = "ni",units = "",vals = c(1:length(lons)), create_dimvar = FALSE )
dimLat <- ncdim_def(name = "nj",units = "",vals = c(1:length(lats)), create_dimvar = FALSE )

varXC <- ncvar_def(name = "xc", units = "degrees_east", dim = list(dimLon,dimLat), longname="longitude", prec="float", missval = NULL)
varYC <- ncvar_def(name = "yc", units = "degrees_north", dim = list(dimLon,dimLat), longname="latitude", prec="float", missval = NULL)

varLon <- ncvar_def(name = "lon", units = "degrees_east", dim = dimLon, longname="longitude", prec="float", missval = NULL)
varLat <- ncvar_def(name = "lat", units = "degrees_north", dim = dimLat, longname="latitude", prec="float", missval = NULL)

varArea <- ncvar_def(name = "area", units = "m2", dim = list(dimLon,dimLat), longname="area of grid cell", prec="float", missval = NULL)
varFrac <- ncvar_def(name = "frac", units = "unitless", dim = list(dimLon,dimLat), longname="fraction of grid cell that is active", prec="float", missval = 0)
varMask <- ncvar_def(name = "mask", units = "unitless", dim = list(dimLon,dimLat), longname="fraction of grid cell that is activedomain mask", prec="float", missval = 0)
varRun_cell <- ncvar_def(name = "run_cell", units = "unitless", dim = list(dimLon,dimLat), longname="fraction of grid cell that is activedomain run_cell", prec="float", missval = 0)

# Create a netCDF file with the variables
ncout <- nc_create( filename = args$output,vars = list(varLon, varLat, varXC, varYC, varArea, varFrac, varMask, varRun_cell) )

# Fill the netCDF variables
ncvar_put( ncout, varLon, lons )
ncvar_put( ncout, varLat, lats )
ncvar_put( ncout, varXC, xcs )
ncvar_put( ncout, varYC, ycs )
ncvar_put( ncout, varArea, areas )
ncvar_put( ncout, varFrac, fracs )
ncvar_put( ncout, varMask, masks )
ncvar_put( ncout, varRun_cell, run_cellss )

# close the netcdf
nc_close(ncout)
