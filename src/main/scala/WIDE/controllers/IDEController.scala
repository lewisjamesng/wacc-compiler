package WIDE.controllers

import LexerSemanticCheck.ParserCombinator
import WIDE.Wide.stage
import com.kodedu.terminalfx.TerminalBuilder
import org.fxmisc.richtext.{CodeArea, LineNumberFactory}
import parsley.{Failure, Success}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.scene.control._
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage._
import scalafxml.core.macros.sfxml

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source

@sfxml
class IDEController(
                     private val tabPane: TabPane,
                     private val outputArea: TextArea,
                     private val outputPane: TabPane,
                     private val treeView: TreeView[File]
) {

  val DEFAULT_FILE_NAME = "main.wacc"
  val DEFAULT_CONTENTS = "begin\n\n# write your code here\n\nend"

  private val tabTextMap = mutable.Map[scalafx.beans.property.StringProperty, CodeArea]()
  private val openFileSet = mutable.Set[String]()
  private val projectFilesMap = mutable.Map[String, File]()

  // Terminal initialisation
  private val terminal = (new TerminalBuilder).newTerminal
  terminal.text = "Terminal"
  outputPane.tabs += terminal

  // File chooser to open and save files
  var dir: File = new File("projectRoot")
  treeView.root = nodesForDirectory(dir)
  private val fileChooser: FileChooser = new FileChooser {
    extensionFilters += new ExtensionFilter("WACC Files", "*.wacc")
  }
  fileChooser.setInitialDirectory(dir)

  def openFileInTab(f: File): Unit = {
    val tab = generateNewTab()
    if (f != null) {
      println("opening file in new tab")
      tab.text = f.getName
      println(readFromFile(f))
      tabTextMap(tab.id).replaceText(readFromFile(f))
      tabPane.tabs += tab
      tabPane.selectionModel().select(tab)
    }
  }

  // Allows files to be opened when tree view is clicked
  treeView.onMouseClicked = _ => {
    try{
      val selectedFile = treeView.selectionModel().getSelectedItem.getValue
      // Open file if it is a file and has not been opened before
      if (!selectedFile.isDirectory && openFileSet.add(selectedFile.getName)) {
        openFileInTab(selectedFile)
      }
      // todo: switch to selected tab
    } catch {
      case _: Exception => println("invalid selection")
    }
  }

  // Builds treeview representation of directory
  private def nodesForDirectory(dir: File): TreeItem[File] = {
    val root = new TreeItem[File](new File(dir.getName))
    dir.listFiles()
      .foreach(f => {
        if (f.isDirectory) root.children += nodesForDirectory(f)
        else if (f.getPath.split("\\.").last.equals("wacc")) {
          projectFilesMap += (f.getName -> f)
          root.children += new TreeItem[File](new File(f.getName))
        }
      })
    root.setExpanded(true)
    root
  }

  // Generate new tab with default values
  private def generateNewTab(): Tab = {
    val tab = new Tab
    val codeArea = new CodeArea
    codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea))
    codeArea.replaceText(DEFAULT_CONTENTS)
    tab.text = DEFAULT_FILE_NAME
    tab.content = codeArea
    codeArea.onKeyTyped = _ => syntaxCheck()
    tab.onCloseRequest = _ => {
      openFileSet.remove(tab.text.value)
      println("saving tab contents")
      saveClick(null)
      treeView.root = nodesForDirectory(dir)
      openFileSet.remove(tab.text.value)
    }
    tabTextMap += (tab.id -> codeArea)
    tab
  }

  private def currentTab: Tab = tabPane.selectionModel().selectedItem()

  // IO functions
  private def saveToFile(content: String, file: File) {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }

  private def readFromFile(file: File): String = {
    val source = Source.fromFile(s"${dir.getName}/${file.getPath}")
    val contents = source.getLines.mkString("\n")
    source.close()
    contents
  }

  // Performs syntax check on current tab's code area
  def syntaxCheck(): Unit = {
    try {
      println("checking syntax...")
      ParserCombinator.programParser.runParser(
      tabTextMap(currentTab.id).getText()
      ) match {
        case Success(_) =>
          outputArea.text = "Passing syntax checks"
        case Failure(msg) =>
          outputArea.text = msg
      }
    } catch {
      case _: Exception => println("syntax checking failed")
    }
  }

  // File menu
  def openClick(e: ActionEvent): Unit = {
    val f = fileChooser.showOpenDialog(stage)
    openFileInTab(f)
    treeView.root = nodesForDirectory(dir)
    syntaxCheck()
  }

  def saveClick(e: ActionEvent): File = {
    try {
      val tabContents = tabTextMap(currentTab.id).getText()

      val file: File = {
        if (currentTab.text.value.equals("main.wacc"))
        fileChooser.showSaveDialog(stage)
        else projectFilesMap(currentTab.text.value)
      }

      // If this is a new file add to the open file set
      if (currentTab.text.value.equals("main.wacc")) {
        openFileSet.add(file.getName)
      }

      if (file != null) {
        saveToFile(tabContents, file)
        currentTab.text.value = file.getName
      }

      treeView.root = nodesForDirectory(dir)
      return file

    } catch {
      case _: Exception =>
        println("no file opened")
    }
    treeView.root = nodesForDirectory(dir)
    null
  }

  def saveAllClick(e: ActionEvent): Unit = {
    tabPane.tabs.foreach(t =>
    try {
        val tabContents = tabTextMap(t.id).getText()
        val file: File =
        if (t.text.value.equals("main.wacc"))
        fileChooser.showSaveDialog(stage)
        else projectFilesMap(currentTab.text.value)

        if (file != null) {
          saveToFile(tabContents, file)
          if (t.text.value.equals("main.wacc")) t.text = file.getName
        }
      } catch {
        case _: Exception =>
          println("no file opened")
      }
    )
    treeView.root = nodesForDirectory(dir)
  }

  def newClick(e: ActionEvent): Unit = {
    tabPane.tabs += generateNewTab()
    treeView.root = nodesForDirectory(dir)
  }

  def runClick(e: ActionEvent): Unit = {
    val file = saveClick(null)
    outputPane.selectionModel().select(terminal)

    if (file != null) {
      terminal.onTerminalFxReady(() => {
        terminal.getTerminal.command(s"bash compile_run_file.sh /${file.getAbsolutePath}\r")
      })
    }
  }

  def exitClick(e: ActionEvent): Unit = {
    Platform.exit()
  }


  // not working yet
  // todo: calculate relative path from System.Propery("user.dir")
  def openFolderClick(e: ActionEvent): Unit = {
    val dirChooser = new DirectoryChooser()
    dirChooser.setInitialDirectory(dir)
    dir = dirChooser.showDialog(stage)
    if (dir == null || !dir.isDirectory) {
      outputArea.text = "Could not open directory"
    } else {
      treeView.root = nodesForDirectory(dir)
    }
  }

  // Edit menu utils
  def cut(e: ActionEvent): Unit = {
    if(tabTextMap(currentTab.id).getSelectedText.nonEmpty) tabTextMap(currentTab.id).cut()
  }
  def copy(e: ActionEvent): Unit = {
    if(tabTextMap(currentTab.id).getSelectedText.nonEmpty) tabTextMap(currentTab.id).copy()
  }
  def paste(e: ActionEvent): Unit = {
    tabTextMap(currentTab.id).paste()
  }

  def delete(e: ActionEvent): Unit = {
    if(tabTextMap(currentTab.id).getSelectedText.nonEmpty) tabTextMap(currentTab.id).replaceSelection("")
  }


}
