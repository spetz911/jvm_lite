/**
 * Created by oleg on 05/07/14.
 *
 */


import org.apache.bcel.Repository
import org.apache.bcel.classfile.ClassParser
import org.apache.bcel.classfile.Code
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.classfile.Method
import org.apache.bcel.generic.InstructionList


object Hello extends App {

  val clazz: JavaClass = new ClassParser("/Users/oleg/dynamite/World.class").parse

  Repository.addClass(clazz)

  val jvm = new JVM()

  jvm.start(clazz)

  val ms = jvm.convertMethods(clazz)
  val mainFun = ms("main")

//  println(mainFun.getCode)



  val mm: Method = clazz.getMethods()(2)
  val cPool = clazz.getConstantPool

  println(cPool)

//  println(clazz)
  val cc: Code = mm.getCode
  val tmp: InstructionList = new InstructionList(cc.getCode)
//  println("= DEBUG =")
//  println(mm)
//  println(mm.getCode)

//  for (is <- tmp.getInstructions.iterator) {
//    println(is.toString)
//  }

}




















