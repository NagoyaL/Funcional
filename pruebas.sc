import Conjuntos Difusos._

def muchoMayorQue(a:Int, m:Int) : ConjDifuso = {
  def mma(x: Int) : Double = {
    if(x<=a) 0.0
    else if (x>a && x <=m) (x-a ).toDouble/(m-a ).toDouble
    else 1.0
  }
  mma
}

def cercanosA(a:Int, k:Int) : ConjDifuso = {
  def ca (x: Int) : Double = {
    val q= 1.0 + k*(x-a)*(x-a)
    1/q
  }
  ca
}

val mm2= muchoMayorQue (2 ,6)
mm2(5)
mm2(3)
mm2(1)
mm2(2)
pertenece (3 ,mm2)
pertenece (8 ,mm2)
val ca3 = cercanosA (3 , 4)
ca3(3)
ca3(4)
ca3(2)
ca3(5)
pertenece (3 , ca3 )
pertenece (8 , ca3 )

val g1 = grande (1 ,10)
val g2 = grande (2 ,10)

g1 (5)
g1 (10)
g1 (100)
g2 (5)
g2 (10)
g2 (100)

val cmm2 = complemento (mm2)
val cca3 = complemento (ca3)
val cg1 = complemento (g1)
val cg2 = complemento (g2)

cmm2(5)
cmm2(3)
cmm2(1)
cmm2(2)
cca3(3)
cca3(4)
cca3(2)
cca3(5)

cg1(5)
cg1(10)
cg1(100)
cg2(5)
cg2(10)
cg2(100)

val mm2_o_ca3 = union (mm2, ca3 )
pertenece (3 ,mm2_o_ca3 )
pertenece (8 ,mm2_o_ca3 )
pertenece (2 ,mm2_o_ca3 )
pertenece (4 ,mm2_o_ca3 )
val mm2_y_ca3 = interseccion(mm2, ca3 )
pertenece (3 ,mm2_y_ca3 )
pertenece (8 ,mm2_y_ca3 )
pertenece (2 ,mm2_y_ca3 )
pertenece (4 ,mm2_y_ca3 )
perttenece (5 ,mm2_y_ca3 )
inclusion (mm2_y_ca3 , mm2_o_ca3 )
inclusion (mm2, ca3)
inclusion (ca3 ,mm2)
inclusion (mm2, muchoMayorQue (3,7))
inclusion (muchoMayorQue (3,7) ,mm2)
igualdad (mm2_y_ca3 , interseccion(ca3 ,mm2))






