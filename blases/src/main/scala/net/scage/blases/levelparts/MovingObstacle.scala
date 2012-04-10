package net.scage.blases.levelparts

import net.scage.support.Vec
import net.scage.support.physics.objects.StaticPolygon

class MovingObstacle(vertices:List[Vec], start_coord: Vec, end_coord:Vec, val speed:Int) extends StaticPolygon(vertices) {

}
