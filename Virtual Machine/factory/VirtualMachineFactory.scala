package factory

//import adapter.ProgramParserAdapter
import bc.{ByteCodeFactory, ByteCodeParser ,MyByteCodeFactory,MyByteCodeParser}
import vendor.{ProgramParser, VendorProgramParser}
import vm.{VirtualMachineParser,VMParser, MyVirtualMachine, VirtualMachine}

/**
 * The `VirtualMachineFactory` follows the *factory pattern*. It provides
 * methods for each of the important parts in this assignment. You must
 * implement each method such that it returns an object of the correct type.
 */
object VirtualMachineFactory {
  // TODO
  def byteCodeFactory: ByteCodeFactory = new MyByteCodeFactory {}
  // TODO
  def vendorParser: ProgramParser = new VendorProgramParser {}
  // TODO
  def byteCodeParser: ByteCodeParser = new MyByteCodeParser {}
  // TODO
  def virtualMachineParser: VirtualMachineParser = new VMParser{}
  // TODO
  def virtualMachine: VirtualMachine = new MyVirtualMachine{}
}
