package object ConjuntosDifusos{
  // Tipo de dato para representar un conjunto difuso
  type ConjDifuso = Int => Double

  def pertenece(elem: Int, s: ConjDifuso) : Double = {
    s(elem)
  }

  // Funcion que crea el conjunto difuso de numeros grandes
  def grande(d: Int, e: Int) : ConjDifuso = {
    // Funcion auxiliar que hace el calculo del grado de pertenencia
    def grandeAux(x: Int) : Double = {
      val y = x.toDouble / (x+d).toDouble
      math.pow(y, e).toDouble
    }
    ConjDifuso(grandeAux)
  }

  def complemento(c: ConjDifuso) : ConjDifuso = (x: Int) => 1 - c(x)




}
