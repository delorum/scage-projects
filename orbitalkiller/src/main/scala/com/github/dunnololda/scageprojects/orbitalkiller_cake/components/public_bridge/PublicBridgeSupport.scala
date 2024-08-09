package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.public_bridge

import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.celestials.{Planet, PlanetWithAir, Star}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.celestials.{CelestialsAware, CelestialsHelper}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.holder.{
  InterfaceHolder,
  InterfaceHolderAware
}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.saveload.{SaveLoadAware, SaveLoadComponent}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.ShipsAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.ships.holder.{ShipsHolder, ShipsHolderAware}
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.system_evolution.SystemEvolutionAware
import com.github.dunnololda.scageprojects.orbitalkiller_cake.physics.system_evolution.SystemEvolution

trait PublicBridgeSupport
  extends InterfaceHolderAware
  with ShipsHolderAware
  with SystemEvolutionAware
  with CelestialsAware
  with ShipsAware
  with SaveLoadAware {
  val publicInterfaceHolder: InterfaceHolder = interfaceHolder

  val publicShipsHolder: ShipsHolder = shipsHolder

  val publicSystemEvolution: SystemEvolution = systemEvolution

  val publicSun: Star = sun
  val publicEarth: PlanetWithAir = earth
  val publicMoon: Planet = moon

  val publicCelestialsHelper: CelestialsHelper = celestialsHelper

  val publicPlayerShip: Ship4 = playerShip

  val publicSaveLoadComponent: SaveLoadComponent = saveLoadComponent
}
