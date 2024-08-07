package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interfaces

trait InterfaceHolderAware {
  protected def interfaceHolder: InterfaceHolder

  implicit val ih: InterfaceHolderAware = this

  trait InterfaceHolderAwareImpl extends InterfaceHolderAware {
    override protected def interfaceHolder: InterfaceHolder = InterfaceHolderAware.this.interfaceHolder
  }
}
