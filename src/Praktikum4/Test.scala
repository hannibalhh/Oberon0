package Praktikum4

object Test extends App {

  // Records -> Ref -> in der zu suchenden Tabelle muss Ÿbergeben werden
  // Procedure (optional)
  
  // Cont -> direkt in Tabelle, bei Cont nichts tun au§er Const Compile

  print(addNewLine("asbas\n") + ":")
  print(addNewLine("asbas") + ":")
  
  def addNewLine(s: String) = {
    if (s.last == '\n')
      s
    else
      s + "\n"
  }

}
