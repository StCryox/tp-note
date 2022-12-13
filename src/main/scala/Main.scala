object Main extends App {

  println(compteMots("     Hello World"));
  def compteMots(chaine: String): Int = chaine.trim() match {
    case "" => 0
    case _ => { 
      if(chaine.head.isUpper) {
          1 + compteMots(chaine.tail)
      } else {
          compteMots(chaine.tail);
          
      }
    }
  }
}

