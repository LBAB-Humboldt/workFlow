#***********************************************************************
# geoServerUploader.R
#***********************************************************************
# This script allows to upload a tiff file into a GeoServer deployment
# And performs some updates into a mongo database
# All variables related to users, passwords and hosts must be given
# externally at execution time, your are not going to found
# sensitive information in this package.
#
# Uriel Alejandro Moreno Ortiz
# umoreno@humboldt.org.co
# 21-08-2019
#***********************************************************************

require('bitops')   #Must be loaded first
require('RCurl')    #Once loaded bitops make sense to load this
require('stringr')  #to use: str_pad
require('gdalUtils') #to use: gdal_translate
require("mongolite") #to connect to the database

geoServerUploader<-function(localConfObj,bioModelObj,sftpConfObj,GeoConfObj,mongoConfObj){
  
  print(paste0("Starting geoServerUploader for ",bioModelObj$theTiff))
  # gdal_translate runs some fixes on tif files in order to be a valid file for geoserver
  #if a raster file does not upload at the first run you should try setting localConfObj$forceFix=TRUE
  if(localConfObj$forceFix){
    print("Using gdal_translate to fix the tif locally")
    gdal_translate(paste0(localConfObj$pathToTiff,bioModelObj$theTiff),
                   paste0(localConfObj$pathToFixedTiff,bioModelObj$theTiff))  
  }else{
    localConfObj$pathToFixedTiff=localConfObj$pathToTiff
  }
  
  
  # Path to the geoserver data files, for a new taxID
  taxIdFolder<-paste0(sftpConfObj$remoteBase,str_pad(bioModelObj$taxID,5,"left",pad="0"))
  
  # First connection to create the folder for given a taxID
  # sftp://user:pass@host/<path>/<file>
  tryCatch(
    ftpUpload(I('Connecting to SFTP- GeoServer BioModelos'),paste0("sftp://",sftpConfObj$sftpUser,":",sftpConfObj$sftpPassword,"@",GeoConfObj$geoServerHost,"/tmp/deleteMe.txt"),postquote =c(paste0('MkDir ',taxIdFolder))),
    error=function(err){return(NULL)}
  )
  
  # Second connection to upload the fixed tiff into new folder
  # sftp://user:pass@host/<path>/<file>
  print(paste0("Uploading to folder ",str_pad(bioModelObj$taxID,5,"left",pad="0")," in ",GeoConfObj$geoServerHost))
  
  ftpUpload(paste0(localConfObj$pathToFixedTiff,bioModelObj$theTiff),paste0("sftp://",sftpConfObj$sftpUser,":",sftpConfObj$sftpPassword,"@",GeoConfObj$geoServerHost,taxIdFolder,'/',bioModelObj$theTiff))
  
  print(paste0("Setting workspace and storages taxid-",str_pad(bioModelObj$taxID,5,"left",pad="0"),":",bioModelObj$modelID," into GeoServer"))
  # Create workspaces for each species
  query1<-paste0("curl -u ",GeoConfObj$gsUser,":",GeoConfObj$gsPassword," -s -S -XPOST -H 'Content-type: text/xml' -d '<workspace><name>taxid-",str_pad(bioModelObj$taxID,5,"left",pad="0"),"</name></workspace>' 'http://",GeoConfObj$geoServerHost,":",GeoConfObj$geoServerPort,"/geoserver/rest/workspaces.xml'")
  res1=system(query1)
  
  #Create storage for tiff
  query2<-paste0("curl -u ",GeoConfObj$gsUser,":",GeoConfObj$gsPassword," -s -S -XPOST -H 'Content-type: text/xml' -d '<coverageStore><name>",bioModelObj$modelID,"</name><workspace>taxid-",str_pad(bioModelObj$taxID,5,"left",pad="0"),"</workspace><enabled>true</enabled><type>GeoTIFF</type><url>models/",str_pad(bioModelObj$taxID,5,"left",pad="0"),"/",bioModelObj$theTiff,"</url></coverageStore>' 'http://",GeoConfObj$geoServerHost,":",GeoConfObj$geoServerPort,"/geoserver/rest/workspaces/taxid-",str_pad(bioModelObj$taxID,5,"left",pad="0"),"/coveragestores'")
  res2=system(query2)
  
  # Setting tif as coverage with json
  jsonFragment=paste0('{
    "coverage": {
      "abstract": "BioModelos Humboldt",
      "defaultInterpolationMethod": "nearest neighbor",
      "description": "Generated from R to BioModelos",
      "enabled": true,
      "interpolationMethods": {
        "string": [
          "nearest neighbor",
          "bilinear",
          "bicubic"
        ]
      },
      "keywords": {
        "string": [
          "',bioModelObj$acceptedNameUsage,'",
          "',bioModelObj$method,'",
          "',bioModelObj$status,'",
        ]
      },
      "name": "',bioModelObj$modelID,'",
      "nativeFormat": "GeoTIFF",
      "requestSRS": {
        "string": [
          "EPSG:4326"
        ]
      },
      "responseSRS": {
        "string": [
          "EPSG:4326"
        ]
      },
      "srs": "EPSG:4326",
      "supportedFormats": {
        "string": [
          "ARCGRID",
          "IMAGEMOSAIC",
          "GTOPO30",
          "GEOTIFF",
          "GIF",
          "PNG",
          "JPEG",
          "TIFF"
        ]
      },
      "title": "',substr(bioModelObj$theTiff,0,str_length(bioModelObj$theTiff)-4),'"
    }
  }')
  
  query3<-paste0("curl -u ",GeoConfObj$gsUser,":",GeoConfObj$gsPassword," -s -S -X POST -H 'Content-type: application/json' -d '",jsonFragment,"' 'http://",GeoConfObj$geoServerHost,":",GeoConfObj$geoServerPort,"/geoserver/rest/workspaces/taxid-",str_pad(bioModelObj$taxID,5,"left",pad="0"),"/coveragestores/",bioModelObj$modelID,"/coverages'")
  res3=system(query3)
  
  print("Updating geoTIFF attribute in database")
  mongoDB <- mongo(mongoConfObj$mongoCollection, url = mongoConfObj$mongoUrl)
  
  where<-paste0('{"modelID":"',bioModelObj$modelID,'"}')
  update<-paste0('{"$set":{"geoTIFF":"',bioModelObj$theTiff,'"}}')
  mongoDB$update(where,update,multiple=FALSE)
  
  print("Process completed successfully...")
  print("Try more models")
}
