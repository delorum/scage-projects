package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.renderer

import com.github.dunnololda.scage.ScageLibD.{DARK_GRAY, DVec, RED, WHITE, appVersion, drawLine, print, windowHeight, windowWidth}
import com.github.dunnololda.scage.handlers.RendererD
import com.github.dunnololda.scageprojects.orbitalkiller.ships.Ship4
import com.github.dunnololda.scageprojects.orbitalkiller_cake.DrawConstants.scale
import com.github.dunnololda.scageprojects.orbitalkiller_cake.Main.drawMapMode
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interface.holder.InterfaceHolder
import com.github.dunnololda.scageprojects.orbitalkiller_cake.components.saveload.SaveLoadComponent
import com.github.dunnololda.scageprojects.orbitalkiller_cake.util.StringFormatUtils.mOrKmOrMKm

class InterfaceRenderer(
    mainScreen: RendererD,
    playerShip: Ship4,
    interfaceHolder: InterfaceHolder,
    saveLoadComponent: SaveLoadComponent) {
  import mainScreen._
  private var renderActionPercents: String = ""
  private var renderActionTimes: String = ""

  actionStaticPeriodIgnorePause(1000) {
    renderActionPercents =
      f"Render/Action ${1.0 * currentRenderTimeMsec / (currentRenderTimeMsec + currentActionTimeMsec) * 100}%.2f%%/${1.0 * currentActionTimeMsec / (currentRenderTimeMsec + currentActionTimeMsec) * 100}%.2f%%"
    renderActionTimes = s"Render/Action $currentRenderTimeMsec msec/$currentActionTimeMsec msec"
  }

  interface {
    if (onPause) print("Пауза", windowCenter.toVec, align = "center", color = WHITE)
    print("F1 - Справка", 20, windowHeight - 40, align = "bottom-left", color = DARK_GRAY)

    if (saveLoadComponent.showGameSavedMessage) {
      print("Игра сохранена", 20, windowHeight - 60, align = "bottom-left", color = RED)
    } else if (saveLoadComponent.showGameLoadedMessage) {
      print("Игра загружена", 20, windowHeight - 60, align = "bottom-left", color = RED)
    } else if (saveLoadComponent.showGameFailedToLoadMessage) {
      print("Не удалось загрузить сохранение", 20, windowHeight - 60, align = "bottom-left", color = RED)
    }

    print(s"сборка $appVersion", windowWidth - 20, windowHeight - 20, align = "top-right", color = DARK_GRAY)
    print(s"FPS/Ticks $fps/$tps", windowWidth - 20, windowHeight - 40, align = "top-right", color = DARK_GRAY)
    print(renderActionPercents, windowWidth - 20, windowHeight - 60, align = "top-right", color = DARK_GRAY)
    print(renderActionTimes, windowWidth - 20, windowHeight - 80, align = "top-right", color = DARK_GRAY)

    val a = DVec(windowWidth - 250, 20)
    val b = DVec(windowWidth - 250 + 100, 20)
    drawLine(a, b, DARK_GRAY)
    drawLine(a, a + (a - b).rotateDeg(90).n * 5, DARK_GRAY)
    drawLine(b, b + (a - b).rotateDeg(90).n * 5, DARK_GRAY)
    print(s"${mOrKmOrMKm((100 / globalScale / (if (drawMapMode) scale else 1.0)).toInt)}", b.toVec, DARK_GRAY)

    interfaceHolder.update(playerShip)
    interfaceHolder.draw(playerShip)
  }
}
