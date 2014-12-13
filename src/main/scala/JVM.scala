/**
 * Created by oleg on 05/07/14.
 *
 */

import org.apache.bcel.classfile._
import org.apache.bcel.{Constants, Repository}

import org.apache.bcel.generic.{InstructionHandle, InstructionList}

import org.apache.bcel.util.ByteSequence
import scala.collection.mutable
import scala.collection.immutable


class JVM {

  val st = new mutable.Stack()
  val ops = new mutable.Stack()

  def interpretation(instruction: String, rest: List[String]) {
    instruction match {
      case "aload_0" => {
        // skip
        println("aload_0")
      }
      case "invokespecial" => {
        println("invokespecial", rest(0), rest(1))
      }
      case "return" => {
        println("return")
      }
    }
  }

  def escape(str: String): String = {
    val len: Int = str.length
    val buf: StringBuffer = new StringBuffer(len + 5)
    val ch: Array[Char] = str.toCharArray
    for (i <- Range(0, len)) {
      ch(i) match {
        case '\n' =>
          buf.append("\\n")
        case '\r' =>
          buf.append("\\r")
        case '\t' =>
          buf.append("\\t")
        case '\b' =>
          buf.append("\\b")
        case '"' =>
          buf.append("\\\"")
        case _ =>
          buf.append(ch(i))
      }
    }
    buf.toString
  }

  /**
   * Resolve constant to a string representation.
   */
  def constantParse(index: Int, tag: Byte, pool: ConstantPool): String = {
    var c: Constant = pool.getConstant(index)
    var str: String = null
    var clazz: JavaClass = null
//    var i: Int = 0
    val tag: Byte = c.getTag
    tag match {
      case Constants.CONSTANT_Class =>
        println("CONSTANT_Class")

        val i = c.asInstanceOf[ConstantClass].getNameIndex
        c = pool.getConstant(i, Constants.CONSTANT_Utf8)
        val cls = Utility.compactClassName(c.asInstanceOf[ConstantUtf8].getBytes, false)
        println("c = " + c)
        val obj: JavaClass = Repository.lookupClass(cls)
        clazz = obj
        str = "cls: " + cls
      case Constants.CONSTANT_String =>
        println("CONSTANT_String")
        val i = c.asInstanceOf[ConstantString].getStringIndex
        c = pool.getConstant(i, Constants.CONSTANT_Utf8)
        str = "\"" + escape(c.asInstanceOf[ConstantUtf8].getBytes) + "\""
      case Constants.CONSTANT_Utf8 =>
        str = c.asInstanceOf[ConstantUtf8].getBytes
      case Constants.CONSTANT_Double =>
        str = "" + c.asInstanceOf[ConstantDouble].getBytes
      case Constants.CONSTANT_Float =>
        str = "" + c.asInstanceOf[ConstantFloat].getBytes
      case Constants.CONSTANT_Long =>
        str = "" + c.asInstanceOf[ConstantLong].getBytes
      case Constants.CONSTANT_Integer =>
        str = "" + c.asInstanceOf[ConstantInteger].getBytes
      case Constants.CONSTANT_NameAndType =>
        println("CONSTANT_NameAndType")
        str = " 1: " + constantParse(c.asInstanceOf[ConstantNameAndType].getNameIndex, Constants.CONSTANT_Utf8, pool)
        str += " 2: \'" + constantParse(c.asInstanceOf[ConstantNameAndType].getSignatureIndex, Constants.CONSTANT_Utf8, pool) + "\'"
      case Constants.CONSTANT_InterfaceMethodref |
           Constants.CONSTANT_Methodref |
           Constants.CONSTANT_Fieldref =>
        println("CONSTANT_InterfaceMethodref | Methodref | Fieldref")
        str = constantParse(c.asInstanceOf[ConstantCP].getClassIndex, Constants.CONSTANT_Class, pool)
        str += "." + constantParse(c.asInstanceOf[ConstantCP].getNameAndTypeIndex, Constants.CONSTANT_NameAndType, pool)
      case _ =>
        throw new RuntimeException("Unknown constant type " + tag)
    }
    str
  }

  def invokeSome(opcode: Short, cls: String, idx0: Int, pool: ConstantPool) = {
    opcode match {
      case Constants.INVOKESTATIC =>
        var c: Int = pool.getConstant(idx0).getTag
        val idx1 = c.asInstanceOf[ConstantCP].getClassIndex
        val c1: Constant = pool.getConstant(idx1)
        val i = c1.asInstanceOf[ConstantClass].getNameIndex
        val c2: Constant = pool.getConstant(i, Constants.CONSTANT_Utf8)
        val cls = Utility.compactClassName(c.asInstanceOf[ConstantUtf8].getBytes, false)
        println("c = " + c2)
        val clazz: JavaClass = Repository.lookupClass(cls)
        val idx2 = c.asInstanceOf[ConstantCP].getNameAndTypeIndex

        c = pool.getConstant(idx2).asInstanceOf[ConstantNameAndType].getNameIndex
        val fun = c.asInstanceOf[ConstantUtf8].getBytes

        c = pool.getConstant(idx2).asInstanceOf[ConstantNameAndType].getSignatureIndex
        val sig = c.asInstanceOf[ConstantUtf8].getBytes

    }



  }

  def printOne(iHandle: InstructionHandle, bytes: ByteSequence, pool: ConstantPool) {
    bytes.skipBytes(iHandle.getPosition)
    val buf = new mutable.StringBuilder
    val opcode: Short = bytes.readUnsignedByte.asInstanceOf[Short]
    buf.append(Constants.OPCODE_NAMES(opcode))

    opcode match {
      case Constants.INVOKESPECIAL | Constants.INVOKESTATIC | Constants.INVOKEVIRTUAL =>
        val index = bytes.readUnsignedShort
        buf.append("\t").
            append(constantParse(index, Constants.CONSTANT_Methodref, pool)).
            append(" (idx: " + index + ")")

      /* Access object/class fields.
       */
      case Constants.GETFIELD |
           Constants.GETSTATIC |
           Constants.PUTFIELD |
           Constants.PUTSTATIC =>
        val index = bytes.readUnsignedShort
        buf.append("\t\t").
            append(constantParse(index, Constants.CONSTANT_Fieldref, pool)).
            append(" (idx: " + index + ")")
      case _ =>
        if (Constants.NO_OF_OPERANDS(opcode) > 0) {
          {
            for (i <- Range(0, Constants.TYPE_OF_OPERANDS(opcode).length)) {
              {
                buf.append("\t\t")
                Constants.TYPE_OF_OPERANDS(opcode)(i) match {
                  case Constants.T_BYTE =>
                    buf.append(bytes.readByte)
                  case Constants.T_SHORT =>
                    buf.append(bytes.readShort)
                  case Constants.T_INT =>
                    buf.append(bytes.readInt)
                }
              }
            }
          }
        } else {
          new Error("fail")
        }
    }

    println("!! " + buf)
  }



  def convertMethods(clazz: JavaClass) = {

    val tmp = for (mm <- clazz.getMethods) yield (mm.getName, mm)
    tmp.toMap
  }


  def start(clazz: JavaClass) {
    val mm = clazz.getMethods()(2)

    val ms = convertMethods(clazz)

    val mainFun = ms("main")

    val code = mm.getCode.getCode
    val pool = clazz.getConstantPool()

    val tmp: InstructionList = new InstructionList(code)

    for (ih: InstructionHandle <- tmp.getInstructionHandles) {
//      interpretation(is.getName, List("dummy0", "dummy1"))
//      println(code(ih.getPosition))
        printOne(ih, new ByteSequence(code), pool)
    }


  }


}
