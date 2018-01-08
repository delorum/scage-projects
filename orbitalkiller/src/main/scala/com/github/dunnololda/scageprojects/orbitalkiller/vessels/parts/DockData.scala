package com.github.dunnololda.scageprojects.orbitalkiller.vessels.parts

import com.github.dunnololda.scageprojects.orbitalkiller.vessels.{PolygonShip, ProxyShip}

case class DockData(dock_to_ship: PolygonShip,
                    our_dp: DockingPoints,
                    other_ship_dp: DockingPoints,
                    proxy_ship: ProxyShip)
