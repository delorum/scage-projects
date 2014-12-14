package com.github.dunnololda.scageprojects.orbitalkiller.tests

import collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.control.TailCalls._

object Test {
  def roll(num_sides:Int):(Int, Int) = {
    (1+(math.random*num_sides).toInt, 1+(math.random*num_sides).toInt)
  }

  val field = Array(
    "G0",   "A1", "CC1", "A2",  "T1", "R1", "B1",  "CH1", "B2", "B3",
    "JAIL", "C1", "U1",  "C2",  "C3", "R2", "D1",  "CC2", "D2", "D3",
    "FP",   "E1", "CH2", "E2",  "E3", "R3", "F1",  "F2",  "U2", "F3",
    "G2J",  "G1", "G2",  "CC3", "G3", "R4", "CH3", "H1",  "T2", "H2"
  )

  def nextX(from:Int, x:String):Int = {
    field.zipWithIndex.drop(from).find(sq => sq._1.startsWith(x)) match {
      case Some((sq, pos)) => pos
      case None =>
        field.zipWithIndex.find(sq => sq._1.startsWith(x)).map(_._2).getOrElse(0)
    }
  }

  var community_chest = util.Random.shuffle(ArrayBuffer[String](
    "Advance to GO",
    "Go to JAIL",
    "", "", "", "", "", "", "", "", "", "", "", "", "", ""))

  var chance = util.Random.shuffle(ArrayBuffer[String](
    "Advance to GO",
    "Go to JAIL",
    "Go to C1",
    "Go to E3",
    "Go to H2",
    "Go to R1",
    "Go to next R",
    "Go to next R",
    "Go to next U",
    "Go back 3 squares",
    "", "", "", "", "", ""))


  val stats = mutable.HashMap[String, Int]()

  def init() {
    stats.clear()
    community_chest = util.Random.shuffle(ArrayBuffer[String](
      "Advance to GO",
      "Go to JAIL",
      "", "", "", "", "", "", "", "", "", "", "", "", "", ""))
    chance = util.Random.shuffle(ArrayBuffer[String](
      "Advance to GO",
      "Go to JAIL",
      "Go to C1",
      "Go to E3",
      "Go to H2",
      "Go to R1",
      "Go to next R",
      "Go to next R",
      "Go to next U",
      "Go back 3 squares",
      "", "", "", "", "", ""))
  }

  def playMonopoly(cur_pos:Int = 0, dice:Int = 6, moves_left:Int):TailRec[Unit] = {
    print(s"$moves_left : ($cur_pos : ${field(cur_pos)}) : ")
    stats(field(cur_pos)) = stats.getOrElseUpdate(field(cur_pos), 0) + 1
    def processStep(next_pos:Int):TailRec[Unit] = {
      print(s"($next_pos : ${field(next_pos)}) : ")
      field(next_pos) match {
        case "CC1" | "CC2" | "CC3" =>
          val cc = community_chest.remove(0)
          community_chest += cc
          print(s"$cc : ")
          cc match {
            case "Advance to GO" =>
              println("Going to G0!")
              playMonopoly(field.indexOf("G0"), dice, moves_left-1)
            case "Go to JAIL" =>
              println("Going to Jail!")
              playMonopoly(field.indexOf("JAIL"), dice, moves_left-1)
            case _ =>
              println("ordinary move")
              playMonopoly(next_pos, dice, moves_left-1)
          }
        case "CH1" | "CH2" | "CH3" =>
          val ch = chance.remove(0)
          chance += ch
          print(s"$ch : ")
          ch match {
            case "Advance to GO" =>
              println("Going to G0!")
              playMonopoly(field.indexOf("G0"), dice, moves_left-1)
            case "Go to JAIL" =>
              println("Going to Jail!")
              playMonopoly(field.indexOf("JAIL"), dice, moves_left-1)
            case "Go to C1" =>
              println("Going to C1!")
              playMonopoly(field.indexOf("C1"), dice, moves_left-1)
            case "Go to E3" =>
              println("Going to E3!")
              playMonopoly(field.indexOf("E3"), dice, moves_left-1)
            case "Go to H2" =>
              println("Going to H2!")
              playMonopoly(field.indexOf("H2"), dice, moves_left-1)
            case "Go to R1" =>
              println("Going to R1!")
              playMonopoly(field.indexOf("R1"), dice, moves_left-1)
            case "Go to next R" =>
              val next_r = nextX(next_pos, "R")
              println(s"Going to next R! : $next_r : ${field(next_r)}")
              playMonopoly(next_r, dice, moves_left-1)
            case "Go to next U" =>
              val next_u = nextX(next_pos, "U")
              println(s"Going to next U! : ($next_u : ${field(next_u)})")
              playMonopoly(next_u, dice, moves_left-1)
            case "Go back 3 squares" =>
              println(s"Going back 3 squares! : (${next_pos-3} : ${field(next_pos-3)})")
              playMonopoly(next_pos-3, dice, moves_left-1)
            case _ =>
              println("ordinary move")
              playMonopoly(next_pos, dice, moves_left-1)
          }
        case _ =>
          println("ordinary move")
          playMonopoly(next_pos, dice, moves_left-1)
      }
    }
    if(moves_left > 0) {
      val (r1, l1) = roll(dice)
      print(s"($r1, $l1) : ")
      if(r1 == l1) {
        val (r2, l2) = roll(dice)
        print(s"($r2, $l2) : ")
        if(r2 == l2) {
          val (r3, l3) = roll(dice)
          print(s"($r3, $l3) : ")
          if(r3 == l3) {
            println("3 Doubles! Going to Jail!")
            playMonopoly(field.indexOf("JAIL"), dice, moves_left-1)
          } else {
            val steps = r3+l3
            val next_pos = (cur_pos+steps) % field.length
            tailcall(processStep(next_pos))
          }
        } else {
          val steps = r2+l2
          val next_pos = (cur_pos+steps) % field.length
          tailcall(processStep(next_pos))
        }
      } else {
        val steps = r1+l1
        val next_pos = (cur_pos+steps) % field.length
        tailcall(processStep(next_pos))
      }
    } else done(println("no moves left!"))
  }

  val digits = List(0,1,2,3,4,5,6,7,8,9)
  val ops = List('+', '-', '*', '/')

  def replace(l:List[Int], number:Int, ind:Int):List[Int] = {
    l.zipWithIndex.map(x => if(x._2 == ind) number else x._1)
  }

  def result(four_number:List[Int], three_ops:List[Char], order:List[Int]):Int = {
    if(four_number.length == 1) four_number.head
    else {
      val op_ind = order.indexOf(1)
      val op = three_ops(op_ind)
      val operand1 = four_number(op_ind)
      val operand2 = four_number(op_ind)
      val ans = op match {
        case '+' => operand1 + operand2
        case '-' => operand1 - operand2
        case '*' => operand1 * operand2
        case '/' => operand1 / operand2
      }
      result(replace(four_number, ans, op_ind), three_ops.filterNot(_ == op), order.filterNot(x => x == 1).map(_-1))
    }
  }
}
