#!/bin/bash

CURRENT_LOCATION=`pwd`
SCAGEPROJECTS_LOCATION=/home/andrey/java/workspace-scala/scage-projects
GITHUBPAGE_SCAGEPROJECTS_LOCATION=/home/andrey/java/workspace-scala/github-page/dunnololda.github.io/scageprojects
#ALL_PROJECTS=('tetris' 'snake' 'scarcanoid' 'life' 'uke' 'runnegun' 'jetflight' 'blases' 'pong' 'spacewar' 'td' 'lightcycles' 'liftdriver' 'rubic' 'game2048')
ALL_PROJECTS=($(ls -I scageprojects.sh -I .gitignore -I .git))

function redeploy {
    local x=$1
    echo "redeploing $x"
    echo "cd ${SCAGEPROJECTS_LOCATION}/$x"
    cd ${SCAGEPROJECTS_LOCATION}/$x
    echo "mvn clean package -Pwebstart -Dmaven.test.skip -q 2>&1 1>/dev/null"
    mvn clean package -Pwebstart -Dmaven.test.skip -q 2>&1 1>/dev/null
    echo "mkdir -p ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x"
    mkdir -p ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x
    echo "rm -rf ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x/*"
    rm -rf ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x/*
    echo "cp -r target/jnlp/* ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x/"
    cp -r target/jnlp/* ${GITHUBPAGE_SCAGEPROJECTS_LOCATION}/$x/
}

function redeployAll {
    for i in "${ALL_PROJECTS[@]}"
    do
        redeploy $i
    done
}

function printHelp {
    echo "Type one of:"
    for i in "${ALL_PROJECTS[@]}"
    do
        echo $i
    done
    echo \
"to redeploy one particular project, or type 'all' to redeploy them all.
Remember to call scage-projects-replacer.scala in dunnololda.github.io/scageprojects after that"
}

PROJECT_NAME="$1"
for i in "${ALL_PROJECTS[@]}"
do
    if [ "$PROJECT_NAME" == "$i" ]; then
        redeploy $PROJECT_NAME
        echo "cd $CURRENT_LOCATION"
        cd $CURRENT_LOCATION
        exit 0
    fi
done

case $PROJECT_NAME in 
    help)
        printHelp
    ;;
    all)
        redeployAll
    ;;
    *)
        printHelp
    ;;
esac

echo "cd $CURRENT_LOCATION"
cd $CURRENT_LOCATION
