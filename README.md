workFlow
========

Códigos para descargar, actualiza, depurar y verificar registros biológicos

# Entorno de desarrollo

Es posible realizar pruebas de estas librerías y otras funcionalidades de R desde contenedores de docker.

```
docker pull rocker/geospatial:3.6.2
docker run --restart always --name <NOMBRE> -e PASSWORD=<CLAVE> -p 8787:8787 -v $HOME/r-biomodelos-workflow:/home/rstudio -d rocker/geospatial:3.6.2
```

Podrá ingresar a rstudio en un navegador web en la url http://0.0.0.0:8787 con el usuario rstudio y la clave configurada.

