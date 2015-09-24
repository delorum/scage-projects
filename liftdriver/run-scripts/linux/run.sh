#!/bin/bash
java -Dscage.properties=${scageproperties} -Djava.library.path=natives -DLWJGL_DISABLE_XRANDR=true -Dfile.encoding=UTF-8 -jar lib/${artifactId}-${version}.jar