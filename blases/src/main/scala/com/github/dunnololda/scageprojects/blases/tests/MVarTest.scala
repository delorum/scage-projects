package com.github.dunnololda.scageprojects.blases.tests

import java.util.Date

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ops._

object Main extends App {
  class MVar[A] extends Actor {
    private var a:Option[A] = None
    private val put_blocked_actors = ArrayBuffer[(Actor, A)]()
    private val take_blocked_actors = ArrayBuffer[Actor]()

    def act() {
      loop {
        a match {
          case Some(presented_a) if(!take_blocked_actors.isEmpty) =>
            val first_blocked_actor = take_blocked_actors.remove(0)
            a = None
            first_blocked_actor ! ("mvar", presented_a)
          case None if(!put_blocked_actors.isEmpty) =>
            val (first_blocked_actor, his_value) = put_blocked_actors.remove(0)
            a = Some(his_value)
            first_blocked_actor ! "filled"
          case _ =>
        }

        react {
          case ("putMVar", new_a:A, actor:Actor) => a match {
            case Some(presented_a) => put_blocked_actors += ((actor, new_a))
            case None =>
              if(put_blocked_actors.isEmpty) {
                a = Some(new_a)
                actor ! "filled"
              }
          }
          case ("takeMVar", actor:Actor) => a match {
            case Some(presented_a) =>
              if(take_blocked_actors.isEmpty) {
                a = None
                actor ! ("mvar", presented_a)
              }
            case None => take_blocked_actors += actor
          }
        }
      }
    }
    start()
  }

  val m = new MVar[Int]

  spawn {
    println("["+new Date()+"] "+"thread 1: putting 5 to the mvar...")
    m ! ("putMVar", 5, self)
    receive {
      case "filled" =>
    }

    println("["+new Date()+"] "+"thread 1: trying to put 6 and blocks...")
    println("["+new Date()+"] "+"thread 1: should be blocked for 3 seconds until the other thread read the mvar")
    m ! ("putMVar", 6, self)
    receive {
      case "filled" =>
    }
    println("["+new Date()+"] "+"thread 1: seems we are not blocked anymore!")
    println("["+new Date()+"] "+"thread 1: now waiting for 3 seconds and put 7")
    Thread.sleep(3000)
    m ! ("putMVar", 7, self)
  }

  spawn {
    println("["+new Date()+"] "+"thread 2: waiting 3 seconds")
    Thread.sleep(3000)
    println("["+new Date()+"] "+"thread 2: taking mvar...")
    m ! ("takeMVar", self)
    receive {
      case ("mvar", i:Int) => println("["+new Date()+"] "+"thread 2: the value taken: "+i)
    }
    println("["+new Date()+"] "+"thread 2: now trying to take another value...")
    m ! ("takeMVar", self)
    receive {
      case ("mvar", i:Int) => println("["+new Date()+"] "+"thread 2: the value taken: "+i)
    }
    println("["+new Date()+"] "+"thread 2: and another one...")
    m ! ("takeMVar", self)
    receive {
      case ("mvar", i:Int) => println("["+new Date()+"] "+"thread 2: the value taken: "+i)
    }
    println("["+new Date()+"] "+"thread 2: ok, we cool, now exit")
    System.exit(0)
  }
}
