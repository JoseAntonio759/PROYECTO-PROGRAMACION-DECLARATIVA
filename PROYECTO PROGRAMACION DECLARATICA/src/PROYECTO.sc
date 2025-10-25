import scala.util.Random

enum Fila(val valor: Int):
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)

enum Columna(val valor: Int):
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M  extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)

case class Posicion(col: Columna, fila: Fila):
  def x: Int = col.valor
  def y: Int = fila.valor

enum Jugador:
  case Liebre
  case Sabuesos

def sortearTurno(): Jugador =
  if Random.nextBoolean() then Jugador.Liebre
  else Jugador.Sabuesos

case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno: Jugador
                 ):
  def ocupadas: Set[Posicion] = sabuesos + liebre

trait TableroJuego:
  def movimientosDesde(p: Posicion): Set[Posicion]
  def posicionInicialLiebre: Posicion
  def posicionesInicialesSabuesos: Set[Posicion]
  def posicionMetaLiebre: Posicion
  def pintarTablero(estado: Estado): Unit
  def esFinPartida(estado: Estado): Option[Jugador]

object TableroClasicoLyS extends TableroJuego:

  val I2M = Posicion(Columna.I2, Fila.M)
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
    D2M -> Set(D1A, D1M, D1B)
  )

  override def movimientosDesde(p: Posicion): Set[Posicion] =
    grafo.getOrElse(p, Set.empty)

  override val posicionInicialLiebre: Posicion = D2M
  override val posicionesInicialesSabuesos: Set[Posicion] = Set(I1A, I2M, I1B)
  override val posicionMetaLiebre: Posicion = I2M

  override def pintarTablero(estado: Estado): Unit =
    ()  // no hace nada de momento

  override def esFinPartida(estado: Estado): Option[Jugador] =
    None  // no hace nada de momento
