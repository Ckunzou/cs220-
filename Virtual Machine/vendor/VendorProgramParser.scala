package vendor
import scala.io.Source
/**
 * Created by Xikun Zou on 10/24/2015.
 */
trait VendorProgramParser extends ProgramParser{
  /**
   * Parses a file representation of a bytecode program
   * into an `InstructionList`.
   *
   * @param file the file to parse
   * @return an instruction list
   */
  override def parse(file: String): InstructionList = {
    var result = Vector[Instruction]()

    for(lines<- Source.fromFile(file).getLines()){
      result = result.++(parseString(lines))
    }
    result
  }

  /**
   * Parses a string representation of a bytecode program
   * into an `InstructionList`.
   *
   * @param string the string to parse
   * @return an instruction list
   */
  override def parseString(string: String): InstructionList = {
    var result = Vector[Instruction]()
    val PreIn:Array[String] = string.split("\n")   // gives a array of instructions
    var i = 0
    while (i< PreIn.length)
    {
      val realIns: Array[String] = PreIn(i).split(' ')   // split each instruction (just for iconst)
      if(realIns.length==2 && realIns(0).equals("iconst"))
        {
          val inst = new Instruction(realIns(0), Vector[Int](realIns(1).toInt))
          result = result.:+(inst)
          i=i+1
        }
      else if(realIns.length ==1)
         {
           val inst = new Instruction(realIns(0), Vector[Int]())
           result = result.:+(inst)
           i=i+1
         }
      else
        throw new InvalidInstructionFormatException("Wrong Format")
      }
    result
  }
}
