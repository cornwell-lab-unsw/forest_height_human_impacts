# Scheffer_2018_global_height

Reproducing and extending results of Scheffer *et al* (2018) http://doi.org/10.1111/gcb.14167

This document describes data sources used.

## Data folder setup

The data folder contains the following folders and files, which are described further below

```
data
├── Scheffer_2018
├── Simard_2011
├── Globcover
├── IFL_2013
├── WDPA_Nov2019-shapefile
├── BAAD
├── treecover
├── wc2
└── wc10
```
(Created using `> fs::dir_tree(path = "data")`)


### Height distributions

Folder: `Scheffer_2018`

Description: Frequency distributions of vegetation height between 60°S and 60°N at 0.5° degree resolution 

Citation: 

  Los, S. et al. 2012.  Vegetation height products between 60° s and 60° n from icesat glasdata. – Geoscientific Model    Development 5: 413–432.

License: 

  CC BY 4.0

Access: 

1. Download zip file from link https://www.geosci-model-dev.net/5/413/2012/gmd-5-413-2012-supplement.zip (direct download, 2.6MB)
2. Extract zip file within `data`, and rename directory from `gmd-5-413-2012-supplement` to `Scheffer_2018`. Alternatively, create a new directory `Scheffer_2018` and place `gmd-5-413-2012-supplement/Field_hist_k2_v1.nc` into the new directory

Files:

```
Scheffer_2018
  └── Field_hist_k2_v1.nc
```

### Canopy height

Folder: `Simard_2011`

Description: Spatial data representing canopy height values at 0.5 degree resolution 

Citation: 

Spatial Data Access Tool (SDAT):
ORNL DAAC. 2017. Spatial Data Access Tool (SDAT). ORNL DAAC, Oak Ridge, Tennessee, USA. Accessed  06/19 . https://doi.org/10.3334/ORNLDAAC/1388

Dataset: 
Simard, Marc, et al. "Mapping forest canopy height globally with spaceborne lidar." Journal of Geophysical Research: Biogeosciences 116.G4 (2011).

License: XXXX

Access: 

1. Visit https://webmap.ornl.gov/ogc/dataset.jsp?ds_id=10023
2. Adjust Resolution (x,y) values to 0.5, 0.5 degrees 
3. Click download (requires EarthData login) (geospatial tiff file, 32.7KB)
4. Copy the downloaded file into `data/Simard 2011`

Files:

```
Simard_2011
└── sdat_10023_1_20190603_003205838.tif
```

### Intact Forest Landscape (IFL)

Folder: `IFL_2013`

Description: Mosaic of forest and natually treeless systems, which exhibit no signs of human activity or habitat degradation, and large enough to native biodiversity. 

Citation: 

Potapov, Peter, et al. "Mapping the world’s intact forest landscapes by remote sensing." Ecology and Society 13.2 (2008).

License: 

CC BY 4.0

Access: 

1. Visit http://www.intactforests.org/data.ifl.html. Under the ESRI SHAPE format section, 
download IFL for the year 2013 (zip file, 141MB)
2. Extract the zip file and copy the folder `IFL_2013` into `data` 

Files:

```
IFL_2013
├── IFL_2018_readme.pdf
├── ifl_2013.cpg
├── ifl_2013.dbf
├── ifl_2013.prj
├── ifl_2013.sbn
├── ifl_2013.sbx
├── ifl_2013.shp
└── ifl_2013.shx
```


### Globcover
 
Folder: `Globcover`

Description: Land-use cover map at 300m resolution 

Citation: 

Defourny, Pierre, et al. "GLOBCOVER: a 300 m global land cover product for 2005 using Envisat MERIS time series." Proceedings of the ISPRS commission VII mid-term symposium: remote sensing: from pixels to processes. 2006.

License: XXXX

Access:

1. Download zip file from link http://due.esrin.esa.int/files/Globcover_V2.2_Global.zip (direct download, 277MB) 
2. Extract zip file, which should produce a folder `Globcover_V2.2_Global`. Rename the folder to `Globcover` and copy into `data`. 

Processing: We also created a downscaled version of the data, using script `Globcover/downscaling.R`, to create `Globcover/downscaled/globcover_downscaled.grd` and `Globcover/downscaled/globcover_downscaled.gri`

Files:

```
Globcover
├── Globcover_V2.2_Global
│   ├── GLOBCOVER_200412_200606_V2.2_Global_CLA.tif
│   ├── GLOBCOVER_200412_200606_V2.2_Global_CLA.tif.vat.dbf
│   ├── GLOBCOVER_200412_200606_V2.2_Global_CLA_QL.tif
│   ├── Globcover_Global_Legend.avl
│   ├── Globcover_Global_Legend.dsr
│   ├── Globcover_Global_Legend.lyr
│   ├── Globcover_Legend.xls
│   └── Globcover_Preview.jpg
└── downscaled
    ├── globcover_downscaled.grd
    └── globcover_downscaled.gri
```

### MODIS 
 
Folder: `treecover`

Description: Spatial data representing percentage tree cover (ratio of the area covered with branches and tree canopy to the ground surface) at 30 arcsecond resolution, obtained from the MODIS sensor 

Citation:

Geospatial Information Authority of Japan, Chiba University and collaborating organizations

License: XXXX

Access: 

1. Download `gm_ve_v1.zip` from https://github.com/globalmaps/gm_ve_v1/blob/master/gm_ve_v1.zip (76MB) 
2. Extract the zip file, and copy the folder `gm_ve_v1` into `data/treecover` 

Processing: We also created a downscaled version of the data, using script `treecover/downscaling.R`, to create `treecover/downscaled/treecover_downscaled.grd` and `treecover/downscaled/treecover_downscaled.gri`

Files:

```
treecover
├── downscaled
│   ├── treecover_downscaled.grd
│   └── treecover_downscaled.gri
└── gm_ve_v1
    └── gm_ve_v1.tif
```

### World Database of Protected Areas (WDPA)

Folder: `WDPA_Nov2019-shapefile`

Description: XXXX

Citation: UNEP, I. 2009. World database on protected areas (wdpa). – Annual release. 

License: XXXX

Access: Downloaded from https://www.protectedplanet.net/ in November 2019. Dataset is updated monthly, so this download represents a snapshot of the database at that time.

Files:

```
WDPA_Nov2019-shapefile
├── Resources_in_English
│   ├── Summary_table_WDPA_attributes.pdf
│   └── WDPA_Manual_1_5_EN_FINAL.pdf
├── WDPA_Nov2019-shapefile-points.cpg
├── WDPA_Nov2019-shapefile-points.dbf
├── WDPA_Nov2019-shapefile-points.prj
├── WDPA_Nov2019-shapefile-points.shp
├── WDPA_Nov2019-shapefile-points.shx
├── WDPA_Nov2019-shapefile-polygons.cpg
├── WDPA_Nov2019-shapefile-polygons.dbf
├── WDPA_Nov2019-shapefile-polygons.prj
├── WDPA_Nov2019-shapefile-polygons.shp
└── WDPA_Nov2019-shapefile-polygons.shx
```


### Precipitation

Folder: `wc2`

Description: Spatial data representing monthly precipitation at 30 arcsecond resolution.

Citation: 

Fick,  S. E. and Hijmans,  R. J. 2017.   Worldclim 2:  new 1-km spatial resolution climatesurfaces for global land areas. – International journal of climatology 37: 4302–4315.

License: XXXX

Access:

1. Download zip file from link http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_30s_prec.zip (direct download, 977MB) 
2. Extract zip file, which should produce a folder `wc2.0_30s_prec`.  Rename the folder to `wc2` and copy it to `data`

Files:

```
wc2
├── readme.txt
├── wc2.0_30s_prec_01.tif
├── wc2.0_30s_prec_02.tif
├── wc2.0_30s_prec_03.tif
├── wc2.0_30s_prec_04.tif
├── wc2.0_30s_prec_05.tif
├── wc2.0_30s_prec_06.tif
├── wc2.0_30s_prec_07.tif
├── wc2.0_30s_prec_08.tif
├── wc2.0_30s_prec_09.tif
├── wc2.0_30s_prec_10.tif
├── wc2.0_30s_prec_11.tif
└── wc2.0_30s_prec_12.tif
```

### BAAD 

Folder: `BAAD`

Description: Biomass And Allometry Database for woody plants

Citation: 

Falster, D. S. et al. 2015.   Baad:  a biomass and allometry database for woody plants.   –Ecology 96: 1445.

License: XXXX

Access: Programatically downloaded using R package devtools 

1. Installation of baad.data by running the following in R 
```
install.packages("devtools")
devtools::install_github("richfitz/datastorr")
devtools::install_github("traitecoevo/baad.data")
```
2. Loading data 
```
dataset <- baad.data::baad_data()
```

Files:

```
BAAD
└── baad_with_map.csv
```

