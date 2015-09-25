#!/bin/bash

CURRENT_LOCATION=`pwd`
SCAGEPROJECTS_LOCATION=/home/andrey/java/workspace-scala/scage-projects
GITHUBPAGE_SCAGEPROJECTS_LOCATION=/home/andrey/java/workspace-scala/github-page/dunnololda.github.io/scageprojects
ALL_PROJECTS=('tetris' 'snake' 'scarcanoid' 'life' 'uke' 'runnegun' 'jetflight' 'blases' 'pong' 'spacewar' 'td' 'lightcycles' 'liftdriver')

function redeploy {
    local x=$1
    echo "redeploing $x"
    cd ${SCAGEPROJECTS_LOCATION}/$x
    mvn clean package -Pwebstart -Dmaven.test.skip -q 2>&1 1>/dev/null
    rm -rf ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x/*
    cp -r target/jnlp/* ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x/
}

function redeployAll {
    for i in "${ALL_PROJECTS[@]}"
    do
        redeploy $i
    done
}

PROJECT_NAME="$1"
case $PROJECT_NAME in
    tetris)
        redeploy $PROJECT_NAME
    ;;    
    snake)
        redeploy $PROJECT_NAME
    ;;    
    scarcanoid)
        redeploy $PROJECT_NAME
    ;;    
    life)
        redeploy $PROJECT_NAME
    ;;    
    uke)
        redeploy $PROJECT_NAME
    ;;    
    runnegun)
        redeploy $PROJECT_NAME
    ;;    
    jetflight)
        redeploy $PROJECT_NAME
    ;;    
    blases)
        redeploy $PROJECT_NAME
    ;;    
    pong)
        redeploy $PROJECT_NAME
    ;;    
    spacewar)
        redeploy $PROJECT_NAME
    ;;    
    td)
        redeploy $PROJECT_NAME
    ;;    
    lightcycles)
        redeploy $PROJECT_NAME
    ;;    
    liftdriver)
        redeploy $PROJECT_NAME
    ;;
    help)
        echo \
"Type one of:
tetris
to redeploy one particular project, or type 'all' to redeploy them all.
Remember to call scage-projects-replacer.scala after that"
    ;;
    *)
        redeployAll
    ;;
esac

cd $CURRENT_LOCATION
