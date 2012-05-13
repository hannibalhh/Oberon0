package Praktikum3
import scala.actors.Actor

object Parse extends Actor with App {
  this.start
  def act {
    OberonParser.start
    OberonParser ! 'Parse
    receive {
      case x => {
        println(x)
        OberonParser ! 'Stop
        exit
      }
    }
  }
}
object Test extends Actor with App {
  this.start
  def act {
    OberonParser.start
    OberonParser ! 'Test
    receive {
      case x => {
        println(x)
        OberonParser ! 'Stop
        exit
      }
    }
  }
}