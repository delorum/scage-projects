package com.github.dunnololda.scageprojects.orbitalkiller_cake.components.interfaces

trait ProtectedInterfaceHolderAware {
  protected def interfaceHolder: InterfaceHolder

  trait ProtectedInterfaceHolderAwareImpl extends ProtectedInterfaceHolderAware {
    protected def interfaceHolder: InterfaceHolder = ProtectedInterfaceHolderAware.this.interfaceHolder
  }
}
