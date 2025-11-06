package Proyecto

import scala.util.Random


enum Fila(val valor: Int):
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)

enum Columna(val valor: Int):
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)


enum Jugador:
  case Liebre
  case Sabuesos



case class Posicion(col: Columna, fila: Fila):
  def x: Int = col.valor
  def y: Int = fila.valor

def sortearTurno(): Jugador =
  if Random.nextBoolean() then Jugador.Liebre
  else Jugador.Sabuesos

case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno: Jugador
                 ):
  def ocupadas: Set[Posicion] =
  sabuesos + liebre



trait TableroJuego:
  def movimientosDesde(p: Posicion): Set[Posicion]
  def posicionInicialLiebre: Posicion
  def posicionesInicialesSabuesos: Set[Posicion]
  def posicionMetaLiebre: Posicion
  def pintarTablero(estado: Estado): Unit
  def esFinPartida(estado: Estado): Option[Jugador]

object TableroClasicoLyS extends TableroJuego:

  val I2M = Posicion(Columna.I2, Fila.M) //creo que seria mejor poner casilla en vez de posicion no se
  val I1A = Posicion(Columna.I1, Fila.A)
  val I1M = Posicion(Columna.I1, Fila.M)
  val I1B = Posicion(Columna.I1, Fila.B)
  val MA  = Posicion(Columna.M,  Fila.A)
  val MM  = Posicion(Columna.M,  Fila.M)
  val MB  = Posicion(Columna.M,  Fila.B)
  val D1A = Posicion(Columna.D1, Fila.A)
  val D1M = Posicion(Columna.D1, Fila.M)
  val D1B = Posicion(Columna.D1, Fila.B)
  val D2M = Posicion(Columna.D2, Fila.M)

  val grafo: Map[Posicion, Set[Posicion]] = Map(
    I2M -> Set(I1A, I1M, I1B),
    I1A -> Set(I2M, I1M, MM, MA),
    I1M -> Set(I2M, I1A, I1B, MM),
    I1B -> Set(I2M, I1M, MM, MB),
    MA  -> Set(I1A, MM, D1A),
    MM  -> Set(I1M, D1M, MA, MB),
    MB  -> Set(I1B, MM, D1B),
    D1A -> Set(MA, MM, D1M, D2M),
    D1M -> Set(D1A, MM, D1B, D2M),
    D1B -> Set(D1M, MM, MB, D2M),
    D2M -> Set(D1A, D1M, D1B),
  )

  override def movimientosDesde(p: Posicion): Set[Posicion] =
    grafo.getOrElse(p, Set.empty)

  override val posicionInicialLiebre: Posicion = D1M //ESTA CAMBIADO POR PRUEBAS
  override val posicionesInicialesSabuesos: Set[Posicion] = Set(MA, MM, MB) //ESTA CAMBIADO POR PRUEBAS
  override val posicionMetaLiebre: Posicion = I2M

  private def pintarNodo(p: Posicion, estado: Estado): String =
    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
    if (estado.liebre == p) s"${ROJO}L${RESET}"
    else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
    else s"${BLANCO}o${RESET}"


  override def pintarTablero(estado: Estado): Unit =
    val s = pintarNodo(_, estado)
    println(s" ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println(" ╱ | \\ | / | \\")
    println(s" ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
    println(" \\ | / | \\ | /")
    println(s" ${s(I1B)}-----${s(MB)}-----${s(D1B)}")


  override def esFinPartida(estado: Estado): Option[Jugador] = {
    None
  }

object Estado:
  def inicial(tablero: TableroJuego): Estado =
    Estado(
      liebre = tablero.posicionInicialLiebre,
      sabuesos = tablero.posicionesInicialesSabuesos,
      turno = sortearTurno()
    )

sealed trait MovimientoFicha:
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    val accesibles = tablero.movimientosDesde(estado.liebre)
    accesibles.diff(estado.ocupadas)

case object MovimientoSabueso extends MovimientoFicha:

  // el metodo no distingue entre sabuesos no se si sirve sino usar ZipWithIndex y FlatMap
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesPorSabueso(tablero, estado).map(_._2)

  def movimientosPosiblesPorSabueso(tableroactual: TableroJuego, estadoactual: Estado): Set[(Posicion, Posicion)] =
    estadoactual.sabuesos.flatMap { sabueso =>
      val accesibles = tableroactual.movimientosDesde(sabueso)

      val libres = accesibles.diff(estadoactual.ocupadas)

      val haciaAdelante = libres.filter(destino => destino.x >= sabueso.x)

      haciaAdelante.map(destino => (sabueso, destino))
    }







object App:
  def main(args: Array[String]): Unit =

    val tablero = TableroClasicoLyS

    val estadoInicial = Estado.inicial(tablero)

    println("\nTurno inicial: " + estadoInicial.turno)
    println("Posición inicial de la liebre: " + estadoInicial.liebre)
    println("Posiciones iniciales de los sabuesos: " + estadoInicial.sabuesos)

    println("\nCasillas ocupadas: " + estadoInicial.ocupadas)


    println("\nMovimientos posibles de la liebre:")
    MovimientoLiebre.movimientosPosibles(tablero, estadoInicial).foreach(println)

    println("\nMovimientos posibles de los sabuesos:")
    MovimientoSabueso.movimientosPosibles(tablero, estadoInicial).foreach(println)

