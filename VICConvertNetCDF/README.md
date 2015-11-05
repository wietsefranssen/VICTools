# VICConvertNetCDF

## How to compile:

Locate the NetCDF libraries on your machine and add them to NETCDF_LIB and NETCDF_INCL.

For example:
`$ export NETCDF_LIB=/usr/include; `
`$ export NETCDF_INCL=/usr/include; `

now type 'make clean' and 'make'
`$ make clean`
`$ make`

Or do it directly:
`$ make clean`
`$ make NETCDF_LIB=/usr/include NETCDF_INCL=/usr/include`

do make debug to include debug symbols
`$ make debug`

## Usage

  `./bin/VICCONVERT VIC2bignetcdf ./JobScripts/fluxesSortMonthly.txt`
