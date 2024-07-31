#!/bin/bash
java \
-Dscage.properties=${scageproperties} \
-Djava.library.path=target/natives/ \
-DLWJGL_DISABLE_XRANDR=true \
-Dfile.encoding=UTF-8 \
-Dfont.file=unifont-8.0.01.ttf \
-Dscreen.splash=resources/images/orbitalkillerSplashWithName.png \
-Dscreen.scagelogo=false \
-Drender.framerate=100 \
-jar lib/orbitalkiller.jar
