package com.gituhb.dunnololda.drivecontrol.tests

import net.scage.ScageLib._

sealed class SeaPartStatus
case object Unknown extends SeaPartStatus
case object Nothing extends SeaPartStatus
case object AdviceStrike extends SeaPartStatus
case object ShipPart extends SeaPartStatus

sealed class ScanStatus
case object Four extends ScanStatus {override def toString = "four"}
case object Three extends ScanStatus {override def toString = "three"}
case object Two extends ScanStatus {override def toString = "two"}
case object One extends ScanStatus {override def toString = "one"}

class SeaPart extends TraceTrait {
  type ChangerType = SeaPart

  // changer type must be the type of actual Trace's child in client code
  def changeState(changer: SeaPart, state: State) {}

  var variants:Int = 0
  var status:SeaPartStatus = Unknown

  // maybe 'changeState' is not the right name..
  def state: State = State()
}

object SeaBattle extends ScageScreenApp("Sea Battle Helper", 1024, 768) {
  private val sea = ScageTracer.create[SeaPart](
    field_from_x = 0,
    field_to_x = 400,
    field_from_y = 0,
    field_to_y = 400,
    init_N_x = 10,
    init_N_y = 10,
    solid_edges = true
  )
  for {
    i <- 0 until sea.N_x
    j <- 0 until sea.N_y
  } sea.addTrace(Vec(i, j), new SeaPart)

  private def translatedCoord(coord:Vec, h_x:Float, h_y:Float):Vec = {
    Vec(coord.x*h_x + h_x/2, coord.y*h_y + h_y/2)
  }

  //center = Vec(sea.N_x*sea.h_x/2, sea.N_y*sea.h_y/2)
  render {
    sea.tracesList.foreach(sea_part =>
      sea_part.status match {
        case Unknown      =>
          drawFilledRectCentered(translatedCoord(sea_part.location, sea.h_x, sea.h_y), sea.h_x-1, sea.h_y-1, DARK_GRAY)
          print(sea_part.variants, translatedCoord(sea_part.location, sea.h_x, sea.h_y), align = "center", color = WHITE)
        case Nothing      => drawFilledRectCentered(translatedCoord(sea_part.location, sea.h_x, sea.h_y), sea.h_x-1, sea.h_y-1, BLUE)
        case ShipPart     => drawFilledRectCentered(translatedCoord(sea_part.location, sea.h_x, sea.h_y), sea.h_x-1, sea.h_y-1, RED)
        case AdviceStrike => drawFilledRectCentered(translatedCoord(sea_part.location, sea.h_x, sea.h_y), sea.h_x-1, sea.h_y-1, GREEN)
        case _            =>
      }
    )
  }

  private var mode = false
  leftMouse(onBtnDown = m => sea.tracesInPoint(m.ix/sea.h_x, m.iy/sea.h_y).foreach(sp => if (sp.status != Unknown) sp.status = Unknown else sp.status = if(mode) Nothing else AdviceStrike))
  rightMouse(onBtnDown = m => sea.tracesInPoint(m.ix/sea.h_x, m.iy/sea.h_y).foreach(sp => if(sp.status != Unknown) sp.status = Unknown else sp.status = ShipPart))

  private var current_scan_status:ScanStatus = Four
  private def giveAdvice() {
    def pewpew(location:Vec, lines:Int):Int = {
      val possible_ships = (for {
        pos <- -lines+1 to 0
        possible_ship_horizontal = sea.tracesNearPoint(location, (pos to pos+lines-1), (0 to 0))
        possible_ship_vertical = sea.tracesNearPoint(location, (0 to 0), (pos to (pos+lines-1)))
      } yield List(possible_ship_horizontal, possible_ship_vertical).filter(_.length == lines)).flatten
      if (location == Vec.zero) {
        possible_ships.foreach(ps => {
          println(ps.map(sp => (sp.location, sp.status)).mkString(" "))
        })
      }
      possible_ships.filter(ps => ps.forall(_.status == Unknown)).length
    }
    current_scan_status match {
      case Four => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = pewpew(sp.location, 4))
      case Three => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = pewpew(sp.location, 3))
      case Two => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = pewpew(sp.location, 2))
      case One => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = 1)
    }
  }
  key(KEY_A, onKeyDown = giveAdvice())
  key(KEY_SPACE, onKeyDown = sea.tracesList.filter(_.status == ShipPart).flatMap(sp => sea.tracesNearPoint(sp.location, -1 to 1)).filter(_.status != ShipPart).foreach(_.status = Nothing))
  key(KEY_1, onKeyDown = current_scan_status = One)
  key(KEY_2, onKeyDown = current_scan_status = Two)
  key(KEY_3, onKeyDown = current_scan_status = Three)
  key(KEY_4, onKeyDown = current_scan_status = Four)
  key(KEY_M, onKeyDown = mode = !mode)
  interface {
    print("current scan status: "+current_scan_status, 20, windowHeight - 20, WHITE)
    print("mode: "+mode, 20, windowHeight - 40, WHITE)
  }
}
