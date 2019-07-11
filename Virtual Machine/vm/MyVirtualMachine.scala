package vm

import bc.ByteCode

/**
 * Created by Xikun Zou on 10/25/2015.
 */
class MyVirtualMachine extends VirtualMachine {
  /**
   * Executes a vector of bytecodes.
   *
   * Note, that this is an "immutable" object. That is, it
   * will return a new virtual machine after executing each
   * of the bytecode objects in the vector.
   *
   * @param bc a vector of bytecodes
   * @return a new virtual machine
   */
    var VMstack = scala.collection.immutable.List[Int]()
  override def execute(bc: Vector[ByteCode]): VirtualMachine = {
    if(bc.length==0)
      this
    else
      execute(executeOne(bc)._1)
  }

  /**
   * Returns the state of the virtual machine stack.
   *
   * The value at index 0 is the value on the top of the stack.
   *
   * @return the state of the stack
   */
  override def state: Vector[Int] =
    if(VMstack.isEmpty)
      throw new MachineUnderflowException("Invlid")
  else
    VMstack.toVector

  /**
   * Pushes an integer value onto the virtual machine stack.
   *
   * @param value the integer to push
   * @return a new virtual machine with the integer `value` pushed
   */
  override def push(value: Int): VirtualMachine = {
    VMstack = VMstack.+:(value)
    this
  }
  /**
   * Executes the next bytecode in the vector of bytecodes.
   *
   * This method only executes a single byte code in the vector of bytecodes.
   * It returns a tuple of the new vector of bytecodes (with the executed
   * bytecode removed) and the new virtual machine.
   *
   * You may assume that `bc` contains at least 1 bytecode.
   *
   * @param bc the vector of bytecodes
   * @return a tuple of a new vector of bytecodes and virtual machine
   */
  override def executeOne(bc: Vector[ByteCode]): (Vector[ByteCode], VirtualMachine) = {
    val vm  = bc(0).execute(this)
    (bc.tail,vm)
  }

  /**
   * Pops an integer value off of the virtual machine stack.
   *
   * @return (i, vm), where i is the integer popped and vm is the
   *         new virtual machine
   */
  override def pop(): (Int, VirtualMachine) = {
    if(VMstack.isEmpty)
      throw new MachineUnderflowException("Invlid")
    else {
      val popedInt = VMstack(0)
      VMstack=VMstack.tail
      (popedInt, this)
    }
  }
}
