# victools

'victools' is an R package with tools for VICWUR.

It will contain the following functions:
* (TODO) `VIC4to5`
    This function will convert the vic 4.x input ascii-files in (not the forcing!) to VICWUR (VIC 5) NetCDF format.

## Installation

### Required R-packages

Before installing victools be sure that the following R-packages are installed
* devtools (to install the package from github)
* ncdf4

### Install victools

```R
library("devtools")
install_github("wietsefranssen/victools")
```
  
## Usage

### Load library

```R
library("victools")
```
  
## OLD Tools

The old vic tools are moved to the oldTools branch and will no longer be maintained

### bin2ascii
You van find the tool in the folder: 

`bin2ascii`

This tool convert binary vic files into ascii

### VICConvertNetCDF
You van find the tool in the folder: 

`VICConvertNetCDF`

This tool convert from/to seperate ascii/bin files of VIC into NetCDF.

### vic_convert
You van find the tool in the folder: 

`vic_convert`

This tool makes a domain NetCDF file for VIC 5.x

### RVICParams2Latlon
You van find the tool in the folder: 

`RVICParams2Latlon`

This R-tool converts a RVIC netcdf-parameter file into a latlon based RVIC netcdf-parameter file
