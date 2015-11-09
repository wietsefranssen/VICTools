# VICConvertNetCDF

## How to compile:

Locate the NetCDF libraries on your machine and add them to NETCDF_LIB and NETCDF_INCL.

For example:
``` shell
export NETCDF_LIB=/usr/include;
export NETCDF_INCL=/usr/include;
```

now type 'make clean' and 'make'
``` shell
make clean
make
```

Or do it directly:
``` shell
make clean
make NETCDF_LIB=/usr/include NETCDF_INCL=/usr/include
```

do make debug to include debug symbols
``` shell
make debug
```
## Usage
``` shell
./bin/VICCONVERT VIC2bignetcdf ./JobScripts/fluxesSortMonthly.txt
```
