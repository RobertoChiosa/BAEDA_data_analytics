#!/bin/bash

ROOT_FOLDER='dashboard-students' # Get root folder name
DATE=$(date '+%Y-%m-%d') # Get current date
LOCAL=false # Set to true if you want to build locally

echo "Building docker image ${ROOT_FOLDER}:${DATE}.v1"

# if local is true, then build locally; else, build online
if [ "$LOCAL" == "true" ]; then
    docker build -t harbor.crownlabs.polito.it/crownlabs-standalone/shinyapp/${ROOT_FOLDER}:${DATE}.v1 .
    docker run -it --rm -p 80:80 harbor.crownlabs.polito.it/crownlabs-standalone/shinyapp/${ROOT_FOLDER}:${DATE}.v1
    exit 0
else
    echo "Login to harbor.crownlabs.polito.it"
    docker login harbor.crownlabs.polito.it
    docker buildx build --push -t harbor.crownlabs.polito.it/crownlabs-standalone/shinyapp/${ROOT_FOLDER}:${DATE}.v1 .
    exit 0
fi
