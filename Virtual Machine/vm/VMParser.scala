package vm

import bc.{InvalidBytecodeException, MyByteCodeParser}
import vendor.VendorProgramParser

import scala.io.Source

/**
 * Created by Xikun Zou on 10/26/2015.
 */
trait VMParser extends VirtualMachineParser with MyByteCodeParser{
  /**
   * Returns a vector of [[bc.ByteCode]].
   *
   * This method parses a file into a vector of bytecode objects.
   * Note, this method should throw a [[bc.InvalidBytecodeException]]
   * if it fails to parse a program file correctly.
   *
   * @param file the file containing a program
   * @return a vector of bytecodes
   */
  override def parse(file: _root_.scala.Predef.String): scala.Vector[_root_.bc.ByteCode] = {
    var result = Vector[_root_.bc.ByteCode]()
    for(lines<- Source.fromFile(file).getLines())
    {
      result = result.++(parseString(lines))
    }
    result
  }

  /**
   * Returns a vector of [[bc.ByteCode]].
   *
   * This method parses a string into a vector of bytecode objects.
   * Note, this method should throw a [[bc.InvalidBytecodeException]]
   * if it fails to parse a program string correctly.
   *
   * @param str a string containing a program
   * @return a vector of bytecodes
   */
  override def parseString(str: _root_.scala.Predef.String): scala.Vector[_root_.bc.ByteCode] = {
       val vpp = new VendorProgramParser {}
       val InsList = vpp.parseString(str)
       var ByteVec = Vector[Byte]()
       var i = 0
       while (i<InsList.length)
         {
           if(InsList(i).name=="iconst")
             {
               ByteVec=ByteVec.:+(bytecode(InsList(i).name))
               ByteVec=ByteVec.:+(InsList(i).args(0).toByte)
               i=i+1
             }
           else if (names.contains(InsList(i).name))
             {
               ByteVec=ByteVec.:+(bytecode(InsList(i).name))
               i=i+1
             }
           else
             throw new InvalidBytecodeException("Invlid ByteCode")

         }
       val mbcp = new MyByteCodeParser {}
       mbcp.parse(ByteVec)

  }
}
