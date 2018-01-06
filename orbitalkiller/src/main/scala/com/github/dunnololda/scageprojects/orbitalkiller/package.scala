package com.github.dunnololda.scageprojects

import com.github.dunnololda.scageprojects.orbitalkiller.colliders.phys2d.{Body => Phys2dBody, BodyList => Phys2dBodyList, Collider => Phys2dCollider, DynamicShape => Phys2dShape, StaticBody => Phys2dStaticBody}

import scala.language.reflectiveCalls

package object orbitalkiller {
  def maxOption[T](l: Seq[T])(implicit o: Ordering[T]): Option[T] = if (l.isEmpty) None else Some(l.max(o))

  def checkAllConditions(conditions: (() => Boolean)*): Boolean = {
    if (conditions.isEmpty) true
    else if (conditions.head()) checkAllConditions(conditions.tail: _*)
    else false
  }
}
