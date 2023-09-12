LOAD httpfs;
LOAD spatial;

-- This will create a GeoJSONSeq file with places in NYC metropolitan and neighboring NJ areas.

COPY (
    SELECT
       id,
       updatetime,
       version,
       CAST(names AS JSON) AS names,
       CAST(categories AS JSON) AS categories,
       confidence,
       CAST(websites AS JSON) AS websites,
       CAST(socials AS JSON) AS socials,
       CAST(emails AS JSON) AS emails,
       CAST(phones AS JSON) AS phones,
       CAST(brand AS JSON) AS brand,
       CAST(addresses AS JSON) AS addresses,
       CAST(sources AS JSON) AS sources,
       ST_GeomFromWKB(geometry)
    FROM
       read_parquet('s3://overturemaps-us-west-2/release/2023-07-26-alpha.0/theme=places/type=*/*', hive_partitioning=1)
    WHERE
        bbox.minx > -74.520264
        AND bbox.maxx < -73.715515
        AND bbox.miny > 40.417678
        AND bbox.maxy < 40.942564
    ) TO 'places_nycnj.geojsonseq'
WITH (FORMAT GDAL, DRIVER 'GeoJSONSeq');

-- Tip: Replace the last 2 lines with:
--
--      ) TO 'places_nycnj.shp'
--   WITH (FORMAT GDAL, DRIVER 'ESRI Shapefile');
--
-- to write the data directly to a shapefile.