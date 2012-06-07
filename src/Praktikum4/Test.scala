package Praktikum4

object Test extends App {

  // Records -> Ref
  // Procedure (optional)

  print(addNewLine("asbas\n") + ":")
  print(addNewLine("asbas") + ":")
  
  def addNewLine(s: String) = {
    if (s.last == '\n')
      s
    else
      s + "\n"
  }

}
