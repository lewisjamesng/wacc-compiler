package WIDE

import javafx.scene.Parent

import java.io.IOException
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafxml.core.{FXMLView, NoDependencyResolver}

object Wide extends JFXApp {
  private val resource = getClass.getResource("resources/IDE.fxml")
  if (resource == null) {
    throw new IOException("Cannot load resource: resources/IDE.fxml")
  }

  val root: Parent = FXMLView(resource, NoDependencyResolver)
  val _scene = new Scene(root)
  _scene.stylesheets += getClass.getResource("resources/style.css").toExternalForm

  stage = new PrimaryStage() {
    title = "WIDE"
    scene = _scene
  }
}
