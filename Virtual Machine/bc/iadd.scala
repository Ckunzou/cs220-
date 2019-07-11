package bc

import vm.VirtualMachine

/**
 * Created by Xikun Zou on 10/25/2015.
 *
 *
 * implement a bytecode for each of the defined bytecodes in the bc.ByteCodeValues trait
 * total 12 classes
 *
 *
 */

//The iconst instruction pushes the integer value NUM on the virtual machine stack.
class iconst(value:Int) extends ByteCode{
  override val code: Byte = bytecode("iconst")
  override def execute(vm : VirtualMachine): VirtualMachine={
    vm.push(value)
    vm
  }
}
//The iadd instruction pops the top two values from the virtual machine stack and pushes the result.
class iadd extends ByteCode{
  override val code: Byte = bytecode("iadd")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<2)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1+ vm.pop()._1)
    vm
  }
}
//The isub instruction pops the top two values from the virtual machine stack and pushes the result.
class isub extends ByteCode{
  override val code: Byte = bytecode("isub")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<2)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1 - vm.pop()._1)
    vm
  }
}
//The imul instruction pops the top two values from the virtual machine stack and pushes the result.
class imul extends ByteCode{
  override val code: Byte = bytecode("imul")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<2)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1 * vm.pop()._1)
    vm
  }
}
//The idiv instruction pops the top two values from the virtual machine stack and pushes the result.
class idiv extends ByteCode{
  override val code: Byte = bytecode("idiv")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<2)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1 / vm.pop()._1)
    vm
  }
}
//The irem instruction pops the top two values from the virtual machine stack and pushes the result.
class irem extends ByteCode{
  override val code: Byte = bytecode("irem")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<2)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1 %  vm.pop()._1)
    vm
  }
}
//The ineg instruction pops the the top value from the virtual machine stack, negates it, and pushes the result.
class ineg extends ByteCode{
  override val code: Byte = bytecode("ineg")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<1)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(-vm.pop()._1)
    vm
  }
}
//The iinc instruction pops the the top value from the virtual machine stack, increments it, and pushes the result.
class iinc extends ByteCode{
  override val code: Byte = bytecode("iinc")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<1)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1+ 1)
    vm
  }
}
//The idec instruction pops the the top value from the virtual machine stack, decrements it, and pushes the result.
class idec extends ByteCode{
  override val code: Byte = bytecode("idec")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<1)
      throw new InvalidBytecodeException("invalid bytecode!")
    vm.push(vm.pop()._1-1)
    vm
  }
}
//The iswap instruction pops the top two values from the virtual machine stack and pushes them in the opposite order
class iswap extends ByteCode{
  override val code: Byte = bytecode("iswap")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<2)
      throw new InvalidBytecodeException("invalid bytecode!")
    val x = vm.pop()._1
    val y = vm.pop()._1
    vm.push(x)
    vm.push(y)
    vm
  }
}
//The idup instruction pops the top value from the stack and pushes it twice onto the stack
class idup extends ByteCode{
  override val code: Byte = bytecode("idup")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<1)
      throw new InvalidBytecodeException("invalid bytecode!")
    val x = vm.pop()._1
    vm.push(x)
    vm.push(x)
    vm
  }
}
//The print instruction pops the top value from the stack and prints the value to the console.
class print extends ByteCode{
  override val code: Byte = bytecode("print")
  override def execute(vm : VirtualMachine): VirtualMachine={
    if(vm.state.size<1)
      throw new InvalidBytecodeException("invalid bytecode!")
    println(vm.pop()._1)
    vm
  }
}

