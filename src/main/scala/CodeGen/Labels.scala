package CodeGen

import scala.collection.mutable
import org.apache.commons.text.StringEscapeUtils

object Labels {

  // Deals with messages under .data e.g:
  // msg_2:
  // 		.word 10
  //  	.ascii	" integers."

  case class DataMsg(s: String, labelIndex: String, actualSize: Int) {
    val label: String = s"msg_$labelIndex"
    val instruction: mutable.ListBuffer[Instruction] =
      mutable.ListBuffer(
        Label(s"msg_$labelIndex"),
        Directive(s"word $actualSize"),
        Directive(s"ascii " + "\"" + s + "\""))
  }

  val dataMsgStore: mutable.LinkedHashMap[String, DataMsg] = mutable.LinkedHashMap.empty
  var dataMsgCounter = 0

  def addDataMsg(s: String): String = {
    val result = addDataMsgWithLabel(s, dataMsgCounter.toString)
    dataMsgCounter += 1
    result
  }

  def addDataMsgWithLabel(s: String, label: String): String = {
    // Get the real length of the string before additional escape chars are added
    val len = s.length
    // Add extra escape chars
    var s_ = StringEscapeUtils.escapeJava(s)
    // Convert scala terminal with ARM terminal
    s_ = s_.replace("\\u0000", "\\0")
    val msg = DataMsg(s_, label, len)
    dataMsgStore.get(s_) match {
      case None =>
        dataMsgStore.put(s_, msg)
        msg.label
      case Some(elem) =>
        elem.label
    }
  }

  private var ifCounter = 0

  def generateIfLabels(): (String, String) = {
    ifCounter += 1
    (s"if_else_${ifCounter - 1}", s"if_end_${ifCounter - 1}")
  }

  private var whileCounter = 0

  def generateWhileLabel(): (String, String) = {
    whileCounter += 1
    (s"while_start_${whileCounter - 1}", s"while_close_${whileCounter - 1}")
  }
}
