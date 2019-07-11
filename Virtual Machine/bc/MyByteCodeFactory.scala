package bc

/**
 * Created by Xikun Zou on 10/25/2015.
 */
trait MyByteCodeFactory extends ByteCodeFactory with ByteCodeValues{
  /**
   * Returns a [[ByteCode]].
   *
   * This method creates a new [[ByteCode]] object given the `byte`
   * that corresponds to the bytecode (see [[ByteCodeValues]]. If
   * the bytecode requires arguments then an optional integer
   * argument is provided.
   *
   * This method should throw an [[InvalidBytecodeException]] if the
   * given bytecode value is unknown.
   *
   * @param byte  the byte code of a bytecode
   * @param args  an optional integer argument (depends on bytecode)
   * @return a new bytecode object
   */
  override def make(byte: Byte, args: Int*): _root_.bc.ByteCode = {
      if(byte==bytecode("iadd")) new iadd
      else if (byte ==bytecode("iconst")) new iconst(args(0))
      else if(byte==bytecode("isub")) new isub
      else if(byte==bytecode("imul")) new imul
      else if(byte==bytecode("idiv")) new idiv
      else if(byte==bytecode("irem")) new irem
      else if(byte==bytecode("ineg")) new ineg
      else if(byte==bytecode("iinc")) new iinc
      else if(byte==bytecode("idec")) new idec
      else if(byte==bytecode("iswap")) new iswap
      else if(byte==bytecode("idup")) new idup
      else if(byte==bytecode("print")) new print
    else
      throw new InvalidBytecodeException("Invlid ByteCode")
  }
}
