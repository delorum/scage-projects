package com.gituhb.dunnololda.drivecontrol.tests

import net.scage.ScageLib._

sealed class SeaPartStatus
case object Unknown extends SeaPartStatus
case object Nothing extends SeaPartStatus
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

object SeaBattle extends ScageScreenApp("Sea Battle Helper", 400, 500) {
  private val sea = ScageTracer.create[SeaPart](
    field_from_x = 40,
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

  private def translatedCoord(coord:Vec, field_from_x:Int = sea.field_from_x, h_x:Float = sea.h_x, h_y:Float = sea.h_y):Vec = {
    Vec(field_from_x + coord.x*h_x + h_x/2, coord.y*h_y + h_y/2)
  }

  //center = Vec(sea.N_x*sea.h_x/2, sea.N_y*sea.h_y/2)
  render {
    List(0,1,2,3,4,5,6,7,8,9).foreach(i => {
      print(i, translatedCoord(Vec(i, sea.N_y)), align = "center", color = WHITE)
    })
    List('А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З', 'И', 'К').zipWithIndex.foreach(c => {
      print(c._1, translatedCoord(Vec(-1, sea.N_y - 1 - c._2)), align = "center", color = WHITE)
    })
    sea.tracesList.foreach(sea_part =>
      sea_part.status match {
        case Unknown      =>
          drawFilledRectCentered(translatedCoord(sea_part.location), sea.h_x-1, sea.h_y-1, DARK_GRAY)
          print(sea_part.variants, translatedCoord(sea_part.location), align = "center", color = WHITE)
        case Nothing      => drawFilledRectCentered(translatedCoord(sea_part.location), sea.h_x-1, sea.h_y-1, BLUE)
        case ShipPart     => drawFilledRectCentered(translatedCoord(sea_part.location), sea.h_x-1, sea.h_y-1, RED)
        case _            =>
      }
    )
  }

  leftMouse(onBtnDown = m => {
    sea.tracesInPoint((m.ix - sea.field_from_x)/sea.h_x, m.iy/sea.h_y).foreach(sp => if (sp.status != Unknown) sp.status = Unknown else sp.status = Nothing)
    giveAdvice()
  })
  rightMouse(onBtnDown = m => {
    sea.tracesInPoint((m.ix - sea.field_from_x)/sea.h_x, m.iy/sea.h_y).foreach(sp => if(sp.status != Unknown) sp.status = Unknown else sp.status = ShipPart)
  })

  private var current_scan_status:ScanStatus = Four
  private var variants:Int = 0
  private var advice:String = ""
  private def giveAdvice() {
    def calculateVariants(location:Vec, lines:Int):Int = {
      (for {
        pos <- -lines+1 to 0
        possible_ship_horizontal = sea.tracesNearPoint(location, (pos to pos+lines-1), (0 to 0))
        possible_ship_vertical = sea.tracesNearPoint(location, (0 to 0), (pos to (pos+lines-1)))
      } yield List(possible_ship_horizontal, possible_ship_vertical).filter(_.length == lines)).flatten.filter(ps => ps.forall(_.status == Unknown)).length
    }
    def location2string(location:Vec):String = {
      ((sea.N_y-1-location.iy) match {
        case 0 => "А"
        case 1 => "Б"
        case 2 => "В"
        case 3 => "Г"
        case 4 => "Д"
        case 5 => "Е"
        case 6 => "Ж"
        case 7 => "З"
        case 8 => "И"
        case 9 => "К"
        case _ => ""
      }) + location.ix
    }
    current_scan_status match {
      case Four  => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = calculateVariants(sp.location, 4))
      case Three => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = calculateVariants(sp.location, 3))
      case Two   => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = calculateVariants(sp.location, 2))
      case One   => sea.tracesList.filter(_.status == Unknown).foreach(sp => sp.variants = 1)
      case _     =>
    }
    variants = sea.tracesList.filter(_.status == Unknown).map(_.variants).sum
    advice = {
      val max_variants = sea.tracesList.filter(_.status == Unknown).map(_.variants).max
      val traces = sea.tracesList.filter(_.status == Unknown).filter(_.variants == max_variants)
      val trace = traces((math.random*traces.length).toInt)
      location2string(trace.location)
    }
  }
  key(KEY_A, onKeyDown = giveAdvice())
  key(KEY_C, onKeyDown = {sea.tracesList.foreach(_.status = Unknown); current_scan_status = Four; giveAdvice()})
  key(KEY_SPACE, onKeyDown = {
    sea.tracesList.filter(_.status == ShipPart).flatMap(sp => sea.tracesNearPoint(sp.location, -1 to 1)).filter(_.status != ShipPart).foreach(_.status = Nothing)
    giveAdvice()
  })
  key(KEY_1, onKeyDown = {current_scan_status = One;   giveAdvice()})
  key(KEY_2, onKeyDown = {current_scan_status = Two;   giveAdvice()})
  key(KEY_3, onKeyDown = {current_scan_status = Three; giveAdvice()})
  key(KEY_4, onKeyDown = {current_scan_status = Four;  giveAdvice()})
  interface {
    print("current scan status: "+current_scan_status, 20, windowHeight - 20, WHITE)
    print("variants: "+variants, 20, windowHeight - 40, WHITE)
    print("advice: "+advice, 20, windowHeight - 60, WHITE)
  }

  giveAdvice()
}
