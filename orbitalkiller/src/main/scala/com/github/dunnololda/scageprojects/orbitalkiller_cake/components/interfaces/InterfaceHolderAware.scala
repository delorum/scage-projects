package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interfaces

trait InterfaceHolderAware {
  def interfaceHolder: InterfaceHolder

  implicit val ih: InterfaceHolderAware = this

  trait InterfaceHolderAwareImpl extends InterfaceHolderAware {
    override def interfaceHolder: InterfaceHolder = InterfaceHolderAware.this.interfaceHolder
  }

  trait ProtectedInterfaceHolderAwareImpl extends ProtectedInterfaceHolderAware {
    protected def interfaceHolder: InterfaceHolder = InterfaceHolderAware.this.interfaceHolder
  }
}
