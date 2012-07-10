#!/bin/bash
java -Dscage.properties=${scageproperties} -Djava.library.path=native -DLWJGL_DISABLE_XRANDR=true -jar lib/${artifactId}-${version}.jar