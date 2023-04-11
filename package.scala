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

  // Funcion para el complemento de un conjunto difuso
  def complemento(c: ConjDifuso) : ConjDifuso = (x: Int) => 1 - c(x)

  // Funcion para hallar la union de dos conjuntos difusos
  def union(cd1: ConjDifuso, cd2: ConjDifuso) : ConjDifuso = {
    (x: Int) => math.max(cd1(x), cd2(x))
  }

  // Funcion para hallar la interseccion de dos conjuntos difusos
  def interseccion(cd1: ConjDifuso, cd2: ConjDifuso) : ConjDifuso = {
    (x: Int) => math.min(cd1(x), cd2(x))
  }

  /*
  Para las siguientes 2 funciones, inclusion e igualdad, se tiene en cuenta el intervalo de los enteros = [0, 1000]
  */

  // Funcion para determinar si un conjunto difuso está incluido en otro
  def inclusion(cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {
    // Funcion que va a iterar sobre los elementos del intervalo [0, 1000]
    def inclAux(x: Int): Boolean = {
      if (x > 1000) true  // Se devuelve true pues si se llega aquí, ya se han revisado todos los enteros del intervalo
      else if (pertenece(x, cd1) > pertenece(x, cd2)) false  // Si el grado de pertenencia de x en cd1 es mayor que el de cd2, entonces no está incluido, se devuelve false y se detiene la iteración
      else inclAux(x + 1)  // Si no, se sigue iterando
    }
    inclAux(0) // Se inicia la iteración en 0, primer entero del intervalo
  }

  // Funcion para determinar si dos conjuntos difusos son iguales
  def igualdad(cd1: ConjDifuso, cd2: ConjDifuso) : Boolean = {
    inclusion(cd1, cd2) && inclusion(cd2, cd1)  // Si un conjunto difuso está incluido en otro, y viceversa, entonces son iguales
  }



}
