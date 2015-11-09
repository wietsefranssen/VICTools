# vic_convert

## usage
`./vic_convert.R`

## show help
`./vic_convert.R -h`

## convert using example data
### Using the total extend of the mask file:
```shell
./vic_convert.R -i ./mask.nc4 -o ./domain.nc4
```
### Using the the extend of a given lonlatbox:
```shell
./vic_convert.R -i ./mask.nc4 -o ./domain.nc4  -s -24.25 39.75 33.25 71.75
```
