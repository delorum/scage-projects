#!/bin/bash
java -Dscage.properties=${scageproperties} -Djava.library.path=natives -DLWJGL_DISABLE_XRANDR=true -Dfile.encoding=UTF-8 -Dfont.file=unifont-8.0.01.ttf -jar lib/orbitalkiller.jar
