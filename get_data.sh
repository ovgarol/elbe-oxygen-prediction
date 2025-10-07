# SPDX-FileCopyrightText: 2025 Helmholtz-Zentrum hereon GmbH
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileContributor Ovidio Garcia-Oliva <ovidio.garcia@hereon.de>

mkdir ./data
cd ./data

# station code for different stations
STATION_CODE=01975 # Hamburg-Fluesbuttel 

## air temperature (TU code) data from DWD
wget -r -nd --no-parent -A "*TU*_${STATION_CODE}_*.zip" https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/

unzip -j "*_akt.zip" "produkt_*.txt"  
unzip -j "*_hist.zip" "produkt_*.txt" 

# data from WSV
NAMES=( "Bunthaus_WGMN!Wassertemperatur" 
        "Seemannshoeft_WGMN!Wassertemperatur" 
        "Blankenese_WGMN!Wassertemperatur"
        "Bunthaus_WGMN!Sauerstoffgehalt_(Einzelmessung)"
        "Seemannshoeft_WGMN!Sauerstoffgehalt_(Einzelmessung)"
        "Blankenese_WGMN!Sauerstoffgehalt_(Einzelmessung)"
      )  

for NAME in "${NAMES[@]}"; do
    FILENAME="$NAME.zip"
    URL="https://www.kuestendaten.de/DE/dynamisch/appl/data/daten_prod/prodNeuProd/direct_download/$FILENAME"
    wget $URL -O $FILENAME
    unzip $FILENAME
    TXTFILE="${NAME}.txt"
    sed -i'' '/^[^0-9]/ s/^/# /' "$TXTFILE" #Comment lines that are not numerical
done

rm *.zip

cd ..
