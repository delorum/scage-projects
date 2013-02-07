package net.scageprojects.befunge

import net.scage.ScageLib._
import javax.swing.JOptionPane

class CharTrace(var char:Char) extends DefaultTrace {

}

object Befunge extends ScageScreenApp("Befunge", 640, 480) {
  commandLineArgAndParse("f", "filename", "befunge source file", has_value = true, required = true)
  val befunge_source_filename = stringProperty("filename")

  val line_arrays = (for {
    line <- io.Source.fromFile(befunge_source_filename).getLines
    line_arr = line.toCharArray
  } yield line_arr).toSeq

  val max_line_length = line_arrays.foldLeft(0) {
    case (len, l) => if(l.length > len) l.length else len
  }

  val arr = (for {
    line_arr <- line_arrays
  } yield line_arr.padTo(max_line_length, ' ')).toArray

  val tracer = ScageTracer.create[CharTrace](
    field_from_x = 30,
    field_to_x   = 30 + max_line_length*20,
    field_from_y = windowHeight - 30 - arr.length*20,
    field_to_y   = windowHeight - 30,
    init_N_x     = max_line_length,
    init_N_y  = arr.length,
    solid_edges = false
  )

  for {
    (line, y) <- arr.zipWithIndex
    (char, x) <- line.zipWithIndex
  } {
    tracer.addTrace(Vec(x, y), new CharTrace(char))
  }

  private var pointer = Vec(0,0)

  private val RIGHT = Vec( 1, 0)
  private val LEFT  = Vec(-1, 0)
  private val UP    = Vec( 0, 1)
  private val DOWN  = Vec( 0,-1)
  private val legit_dirs = List(RIGHT, LEFT, UP, DOWN)
  private var dir = RIGHT

  private def moveInDir(new_dir:Vec) {
    require(legit_dirs.contains(new_dir))
    dir = new_dir
    pointer = tracer.outsidePoint(pointer + dir)
  }
  private def moveInCurrentDir() {moveInDir(dir)}
  private def moveRight() {moveInDir(RIGHT)}
  private def moveLeft() {moveInDir(LEFT)}
  private def moveUp() {moveInDir(UP)}
  private def moveDown() {moveInDir(DOWN)}



  private val stack = collection.mutable.Stack[Int]()


  interface {
    print("Stack:", 30, 110, color = WHITE)
    print(stack.take(10).mkString(" "), 30, 90, color = WHITE, align = "ycenter")
  }

  private var symbolic_mode = false
  action(1000) {
    val char = tracer.tracesInPoint(pointer).head.char
    if(char != '"' && symbolic_mode) {
      stack.push(char)
      moveInCurrentDir()
    } else {
      char match {
        case '>' => moveRight()
        case '<' => moveLeft()
        case '^' => moveUp()
        case 'v' => moveDown()
        case '_' =>
          val elem_on_top = stack.pop()
          if(elem_on_top == '0') moveRight() else moveLeft()
        case '|' =>
          val elem_on_top = stack.pop()
          if(elem_on_top == '0') moveDown() else moveUp()
        case '?' =>
          (math.random*4).toInt match {
            case 0 => moveRight()
            case 1 => moveLeft()
            case 2 => moveUp()
            case 3 => moveDown()
          }
        case '#' =>
          moveInCurrentDir()
          moveInCurrentDir()
        case '@' => deleteSelf()
        case ':' =>
          val elem_on_top = stack.pop()
          stack.push(elem_on_top)
          moveInCurrentDir()
        case '\\' =>
          val elem_on_top = stack.pop()
          val next_after_top = stack.pop()
          stack.push(elem_on_top)
          stack.push(next_after_top)
          moveInCurrentDir()
        case '$' =>
          stack.pop()
          moveInCurrentDir()
        case 'p' =>
          val x = stack.pop()
          val y = stack.pop()
          val char = stack.pop()
          tracer.tracesInPoint(x, y).head.char = char.toChar
          moveInCurrentDir()
        case 'g' =>
          val x = stack.pop()
          val y = stack.pop()
          val char = tracer.tracesInPoint(x, y).head.char
          stack.push(char)
          moveInCurrentDir()
        case num @ ('0' | '1' | '2' | '3' |'4' | '5' |'6' | '7' |'8' | '9') =>
          stack.push(num.asDigit)
          moveInCurrentDir()
        case '"' =>
          symbolic_mode = !symbolic_mode
          moveInCurrentDir()
        case '+' =>
          val a = stack.pop()
          val b = stack.pop()
          stack.push(a+b)
          moveInCurrentDir()
        case '-' =>
          val a = stack.pop()
          val b = stack.pop()
          stack.push(b-a)
          moveInCurrentDir()
        case '*' =>
          val a = stack.pop()
          val b = stack.pop()
          stack.push(a*b)
          moveInCurrentDir()
        case '/' =>
          val a = stack.pop()
          val b = stack.pop()
          stack.push(b/a)
          moveInCurrentDir()
        case '%' =>
          val a = stack.pop()
          val b = stack.pop()
          stack.push(b%a)
          moveInCurrentDir()
        case '!' =>
          val a = stack.pop()
          if(a == 0) stack.push(1) else stack.push(0)
          moveInCurrentDir()
        case '`' =>
          val a = stack.pop()
          val b = stack.pop()
          if(b>a) stack.push(1) else stack.push(0)
          moveInCurrentDir()
        case '&' =>
          val input_number = JOptionPane.showInputDialog("Input number")
          stack.push(input_number.toInt)
          moveInCurrentDir()
        case '~' =>
          val input_symbol = JOptionPane.showInputDialog("Input symbol")
          stack.push(input_symbol.toCharArray.head.toInt)
          moveInCurrentDir()
        case '.' =>
          val char = stack.pop()
          output(char)
          moveInCurrentDir()
        case ',' =>
          val char = stack.pop()
          output(char.toChar)
          moveInCurrentDir()
        case _ =>
          moveInCurrentDir()
      }
    }
  }

  private val output_buf = new StringBuilder()
  private var _output = ""
  private def output(s:Any) {
    output_buf.append(s.toString)
    _output = output_buf.toString()
  }
  interface {
    print("Output:", 30, 50, color = WHITE)
    print(_output,   30, 30, color = WHITE, align = "ycenter")
  }

  interface {
    drawTraceGrid(tracer, DARK_GRAY)
    for {
      char_trace <- tracer.tracesList
    } print(char_trace.char, tracer.field_from_x + char_trace.location.x*tracer.h_x + tracer.h_x/2f,
                             tracer.field_to_y - (char_trace.location.y*tracer.h_y + tracer.h_y/2f), color = WHITE, align = "center")

    drawRectCentered(Vec(tracer.field_from_x + pointer.x*tracer.h_x + tracer.h_x/2f,
                         tracer.field_to_y - (pointer.y*tracer.h_y + tracer.h_y/2f)), tracer.h_x+1, tracer.h_y+1, color = RED)
  }
}
