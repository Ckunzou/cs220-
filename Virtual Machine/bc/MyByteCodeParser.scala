package bc

/**
 * Created by Xikun Zou on 10/25/2015.
 */
trait MyByteCodeParser extends ByteCodeParser{
  /**
   * Parses a vector of `Byte` into a vector of `ByteCode`.
   *
   * You should use [[ByteCodeValues.bytecode]] to help translate
   * the individual `Byte`s into a correponding [[ByteCode]].
   *
   * @param bc  a vector of bytes representing bytecodes
   * @return    a vector of `ByteCode` objects
   */
  override def parse(bc: scala.Vector[Byte]): scala.Vector[_root_.bc.ByteCode] = {
    var resultVec = Vector[_root_.bc.ByteCode]()
    val factory = new MyByteCodeFactory {}
        var i= 0
    while(i<bc.length)
      {
        if(bc(i)==bytecode("iconst"))
        {
          resultVec = resultVec.:+ (factory.make(bc(i),bc(i + 1).toInt))
          i=i+1           // skip a iteration
        }
        else
          {
            resultVec = resultVec.:+(factory.make(bc(i)))
          }
        i=i+1
      }
    resultVec
  }
}
