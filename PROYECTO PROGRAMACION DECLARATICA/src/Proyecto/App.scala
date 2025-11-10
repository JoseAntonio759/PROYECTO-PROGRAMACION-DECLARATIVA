package Proyecto

import Proyecto.MovimientoLiebre.movimientosPosibles

import scala.util.Random


enum Fila(val valor: Int): //crear enum para filas
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)

enum Columna(val valor: Int): //crear enum para columnas
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)


enum Jugador: //enum para distinguir a los jugadores
  case Liebre
  case Sabuesos

case class Posicion(col: Columna, fila: Fila):  //crear clase para distinguir cada posiciones
  def x: Int = col.valor
  def y: Int = fila.valor

  def manhattan(other: Posicion): Int = {
    Math.abs(x - other.x) + Math.abs(y - other.y)
  }

def sortearTurno(): Jugador = //sortear turno del jugador mediante Random.nextBoolean() que da True o False, y luego asignar a cada jugador
  if Random.nextBoolean() then Jugador.Liebre
  else Jugador.Sabuesos

case class Estado( //clase que devuelve el conjunto de posiciones ocupadas en el tablero, al igual que la posicion de la liebre y los sabuesos
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

  val I2M = Posicion(Columna.I2, Fila.M) //crear el tablero y unir cada casilla como un grafo
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

  override def movimientosDesde(p: Posicion): Set[Posicion] = //implementar el metodo del trait, y usar el grafo para obtener los movimientos posibles
    grafo.getOrElse(p, Set.empty)

  override val posicionInicialLiebre: Posicion = D2M //posicion inicial de la liebre
  override val posicionesInicialesSabuesos: Set[Posicion] = Set(I1A, I2M, I1B) //posiciones iniciales de los sabuesos
  override val posicionMetaLiebre: Posicion = I2M //meta de la liebre

  private def pintarNodo(p: Posicion, estado: Estado): String = //metodo para pintar cada casilla del tablero
    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
    if (estado.liebre == p) s"${ROJO}L${RESET}"
    else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
    else s"${BLANCO}o${RESET}"


  override def pintarTablero(estado: Estado): Unit = //metodo para pintar el tablero
    val s = pintarNodo(_, estado)
    println(s"         ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println("      ╱  |  \\  |  /  |  \\")
    println(s"     ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
    println("      \\  |  /  |  \\  |  /")
    println(s"         ${s(I1B)}-----${s(MB)}-----${s(D1B)}")


  override def esFinPartida(estado: Estado): Option[Jugador] = { //metodo que devuelve el ganador del juego si existe, sino devuelve None
    val xLiebre = estado.liebre.x
    val xSabuesos = estado.sabuesos.map(_.x)

    if xLiebre < xSabuesos.min then Some(Jugador.Liebre) //victoria liebre

    else
      val movimientosLiebre = MovimientoLiebre.movimientosPosibles(this, estado) //victoria sabueso
      if movimientosLiebre.isEmpty then Some(Jugador.Sabuesos)
      else
        None
  }

object Estado: // crea el estado inicial del juego a partir de un tablero dado
  def inicial(tablero: TableroJuego): Estado =
    Estado(
      liebre = tablero.posicionInicialLiebre,
      sabuesos = tablero.posicionesInicialesSabuesos,
      turno = sortearTurno()
    )

sealed trait MovimientoFicha: //implementa el concepto de movimiento de ficha, y da los movimeintos posibles desde un estado concreto
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha: //implementa el movimiento de la liebre que viene de MovimientoFicha
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    val accesibles = tablero.movimientosDesde(estado.liebre)
    accesibles.diff(estado.ocupadas)

case object MovimientoSabueso extends MovimientoFicha: //implementa el movimeinto de los sabuesos

  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesPorSabueso(tablero, estado).map(_._2)

  def movimientosPosiblesPorSabueso(tableroactual: TableroJuego, estadoactual: Estado): Set[(Posicion, Posicion)] = //creamos el metodo que devuelva dos posiciones, una que sea en la que esta el sabueso y las otras que sean los posibles destinos
    estadoactual.sabuesos.flatMap { sabueso =>
      val accesibles = tableroactual.movimientosDesde(sabueso)
      val libres = accesibles.diff(estadoactual.ocupadas)
      val haciaAdelante = libres.filter(destino => destino.x >= sabueso.x) //aqui se limita el movimiento de los sabuesos
      haciaAdelante.map(destino => (sabueso, destino))
    }

// Cambia la posición de la liebre y sabuesos dependiendo del movimiento al cual se desplazan
def ejecutarMovimientos(tablero: TableroJuego, estado: Estado, movimiento: Posicion): Estado = estado.turno match
  // Actualizamos el estado: La liebre cambia de posición, el estado de los sabuesos no cambia y el turno se mueve a los mismos.
  case Jugador.Liebre => Estado (
    liebre = movimiento,
    sabuesos = estado.sabuesos,
    turno = Jugador.Sabuesos
  )
  // Actualizamos el estado: La liebre se queda igual, los sabuesos no movidos se quedan igual menos el que no que
  // se quita de su posición inicial para desplazarlo a su movimiento elegido
  case Jugador.Sabuesos =>

    val SabuesoMovido = estado.sabuesos.filter { sabuesos =>
      tablero.movimientosDesde(sabuesos).contains(movimiento)
    }.head

    Estado (
      liebre = estado.liebre,
      sabuesos = estado.sabuesos - SabuesoMovido + movimiento,
      turno = Jugador.Liebre
    )

// Comprobamos que la liebre rebasa a algún sabueso si el set de los estados de los sabuesos condicionado porque la posición de la liebre
// en la x es menor o igual que la posición x de los sabuesos
def liebreHaRebasado(estado: Estado): Boolean = {
  !estado.sabuesos.filter(sabuesos => estado.liebre.x <= sabuesos.x).isEmpty
}

// Por la condición de la liebre rebasa a los sabuesos, comprobamos el tamaño del set como sabuesos rebasados
def sabuesosRebasados(estado: Estado, destino: Posicion): Int = {
  estado.sabuesos.filter(sabuesos => estado.liebre.x <= sabuesos.x).size
}

// Calculamos la suma de las distancias manhattan de la liebre a los sabuesos
def distanciaLiebreSabuesos(liebre: Posicion, sabuesos: Set[Posicion]): Int = {
  sabuesos.map(sabuesos => liebre.manhattan(sabuesos)).sum
}

// Si la liebre la tratamos modoIA, se evalúan sus movimientos con una tupla de dos enteros según la heurística de que
def evaluarMovimientoLiebreIA(tablero: TableroJuego, estadoActual: Estado, destino: Posicion): (Int, Int) = {
  val rebasado = liebreHaRebasado(estadoActual)

  val distancia = distanciaLiebreSabuesos(destino, estadoActual.sabuesos)
  // si la liebre no rebasa sabuesos devuelve los que se rebasan con ese movimiento y la distancia a estos,
  if (!rebasado) {
    val Rebasados = sabuesosRebasados(estadoActual, destino)
    (Rebasados, distancia)
  } else {
    // en caso contrario, devuelve la distancia de la liebre a la meta y a los sabuesos, respectivamente
    val metrica = destino.manhattan(tablero.posicionMetaLiebre)
    (metrica, distancia)
  }
}

// Si los sabuesos los tratamos modoIA, evaluamos sus movimientos con una tupla de 2 enteros con la heurística correspondiente
def evaluarMovimientoSabuesosIA(tablero: TableroJuego, estadoActual: Estado, destino: Posicion): (Int, Int) = {
  val liebreHaRebasadoSabuesos = liebreHaRebasado(estadoActual)

  val sabuesoMovido = estadoActual.sabuesos.filter(s => tablero.movimientosDesde(s).contains(destino)).head

  val estadoPosterior = Estado (
    liebre = estadoActual.liebre,
    sabuesos = estadoActual.sabuesos - sabuesoMovido + destino,
    turno = Jugador.Liebre
  )
  val MovimientosLiebre = MovimientoLiebre.movimientosPosibles(tablero, estadoPosterior).size
  // si la liebre no rebasó sabuesos, se devuelve la distancia de los sabuesos a la liebre y los movimientos de la liebre
  if (!liebreHaRebasadoSabuesos) {
    val distancia = destino.manhattan(estadoActual.liebre)

    (distancia, MovimientosLiebre)
  } else {
    // si la liebre rebasó sabuesos, se devuelve la distancia de los sabuesos a la meta (cuanta más mejor) y los movimientos posibles de la liebre, respectivamente
    val distanciaSabuesosMeta = destino.manhattan(tablero.posicionMetaLiebre)

    (-distanciaSabuesosMeta, MovimientosLiebre)
  }
}

// Iniciamos el juego con el tablero y estado iniciales, y pedimos al jugador que responda que jugadores serán dirigidos por IA
def iniciarJuego(): Unit = {
  val tablero = TableroClasicoLyS
  val estadoInicial = Estado(
    liebre = tablero.posicionInicialLiebre,
    sabuesos = tablero.posicionesInicialesSabuesos,
    turno = sortearTurno()
  )

  println("¿Activar el modo IA (Ninguno / Sabuesos / Liebre / Ambos)?: ")
  val respuesta = scala.io.StdIn.readLine().toLowerCase
  val modoIA = respuesta match {
    case "liebre" => Set(Jugador.Liebre)
    case "sabuesos" => Set(Jugador.Sabuesos)
    case "ambos" => Set(Jugador.Liebre, Jugador.Sabuesos)
    case _ => Set()
  }

  println(s"Comienza el: ${estadoInicial.turno}")
  // Se iterará la función bucle juego sobre el tablero, estado e IA hasta que haya un ganador
  bucleJuego(tablero, estadoInicial, modoIA)

  println("¿Se desea realizar otra partida? (si/no): ")
  val repetir = scala.io.StdIn.readLine().toLowerCase()
  if (repetir == "si") then iniciarJuego()
}

// Definimos el bucle del Juego que termina si la función esFinPartida devuelve un ganador
def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Set[Jugador]): Jugador = {
  // Pintamos el tablero
  tablero.pintarTablero(estado)

  // Definimos los movimientos posibles para liebre y sabuesos, distinguiendo origen y destino
  val movimientosPosibles = estado.turno match {
    case Jugador.Liebre => MovimientoLiebre.movimientosPosibles(tablero, estado).map(destino => (estado.liebre, destino))
    case Jugador.Sabuesos => MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado)
  }

  // Convertimos los movimientos posibles a lista para poder operar con ellos
  val movimientosLista = movimientosPosibles.toList

  // Se evalúan, ordenan decrecientemente y eligen los movimientos correspondientes a cada jugador contenido en modoIA dependiendo de su turno
  if (modoIA.contains(estado.turno)) {
    println(s"Turno de ${estado.turno}")

    val movimientosEvaluados = movimientosLista.map { case (origen, destino) =>
      val valor = estado.turno match {
        case Jugador.Liebre =>
          evaluarMovimientoLiebreIA(tablero, estado, destino)

        case Jugador.Sabuesos =>
          evaluarMovimientoSabuesosIA(tablero, estado, destino)
      }
      ((origen, destino), valor)
    }


    val ordenarMovimientos = movimientosEvaluados.sortBy {
      case (mov, (condicion1, condicion2)) => (-condicion1, -condicion2)
    }

    println("Movimientos posibles evaluados: ")
    ordenarMovimientos.zipWithIndex.foreach { case (((origen, destino), (rebasados, distancia)), num) =>
      println(s"${num}: ${estado.turno} va de ${origen} a ${destino} -> Evaluado: (${rebasados}, ${distancia})")
    }
    // Destino
    val movimientoElegidoIA = ordenarMovimientos.head._1._2

    println(s"Movimiento elegido: ${movimientoElegidoIA}")
    // Actualizamos el estado del jugador por cada movimiento realizado
    val actualizarEstadoIA = ejecutarMovimientos(tablero, estado, movimientoElegidoIA)

    tablero.esFinPartida(actualizarEstadoIA) match {
      case Some(ganador) =>
        tablero.pintarTablero(actualizarEstadoIA)
        println(s"Gana el ${ganador}")
        ganador
      case None =>
        bucleJuego(tablero, actualizarEstadoIA, modoIA)
    }
  } else {
    // En caso de no estar contenido el jugador en la IA, se ordenan y eligen los movimientos a gusto del usuario sin ser evaluados
    println(s"Turno de ${estado.turno}")
    println("Movimientos posibles")
    movimientosLista.zipWithIndex.foreach { case ((origen, destino), num) =>
      println(s"${num}: ${estado.turno} va de ${origen} a ${destino}")
    }

    println("Elija movimiento: ")
    val eleccion = scala.io.StdIn.readLine().toInt
    val elegido = movimientosLista(eleccion)

    // Destino se elige para ejecutar
    val actualizarEstado = ejecutarMovimientos(tablero, estado, elegido._2)

    tablero.esFinPartida(actualizarEstado) match {
      case Some(ganador) =>
        tablero.pintarTablero(actualizarEstado)
        println(s"Gana el ${ganador}")
        ganador
      case None =>
        bucleJuego(tablero, actualizarEstado, modoIA)
    }
  }
}



// Programa principal
object App:
  def main(args: Array[String]): Unit =

    iniciarJuego()
