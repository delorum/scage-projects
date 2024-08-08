package com.github.dunnololda.scageprojects.orbitalkiller_cake.ships.docking

import com.github.dunnololda.scageprojects.orbitalkiller.PolygonShip
import com.github.dunnololda.scageprojects.orbitalkiller.ships.ProxyShip

case class DockData(
    dock_to_ship: PolygonShip,
    our_dp: DockingPoints,
    other_ship_dp: DockingPoints,
    proxy_ship: ProxyShip)
