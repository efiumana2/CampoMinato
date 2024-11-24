package CampoMinato

import com.raquo.laminar.api.L.*
import scala.util.Random
import org.scalajs.dom

// Definizione della cella
case class Cell(
                 hasMine: Boolean,
                 revealed: Var[Boolean],
                 flagged: Var[Boolean],
                 adjacentMines: Int
               )

// Variabile per monitorare lo stato del gioco (se è terminato)
val gameOver: Var[Boolean] = Var(false)

// Funzione per creare una griglia con mine e conteggi di mine adiacenti
def createGrid(gridSize: Int, numMines: Int): List[List[Cell]] = {
  // Crea una griglia vuota di celle
  val emptyGrid = List.fill(gridSize, gridSize)(
    Cell(hasMine = false, revealed = Var(false), flagged = Var(false), adjacentMines = 0)
  )

  // Piazza le mine nella griglia
  val gridWithMines = placeMines(emptyGrid, numMines)

  // Aggiorna il conteggio delle mine vicine
  updateAdjacentMineCounts(gridWithMines)
}

// Funzione per piazzare le mine in posizioni casuali
def placeMines(grid: List[List[Cell]], numMines: Int): List[List[Cell]] = {
  val random = new Random()

  // Crea una lista di tutte le posizioni della griglia
  val allPositions = for {
    row <- grid.indices       // torna un Range della lista
    col <- grid.head.indices
  } yield (row, col)

  // Seleziona posizioni casuali per piazzare le mine
  val minePositions = random.shuffle(allPositions).take(numMines) //mescola le posizioni e prende le prime numMines

  // Crea una nuova griglia con le mine piazzate
  grid.zipWithIndex.map { case (row, rowIndex) =>           // tupla di 2 valore i: il contenuto e l'indice - Torna una lista
    row.zipWithIndex.map { case (cell, colIndex) =>
      if (minePositions.contains((rowIndex, colIndex))) {
        cell.copy(hasMine = true) // Piazza la mina
      } else {
        cell
      }
    }
  }
}

// Funzione per aggiornare il conteggio delle mine vicine
def updateAdjacentMineCounts(grid: List[List[Cell]]): List[List[Cell]] = {
  grid.zipWithIndex.map { case (row, rowIndex) =>
    row.zipWithIndex.map { case (cell, colIndex) =>
      // Ottieni tutte le celle adiacenti
      val adjacentCells = getAdjacentCells(grid, rowIndex, colIndex)

      // Conta quante delle celle adiacenti hanno una mina
      val mineCount = adjacentCells.count(_.hasMine)

      // Aggiorna la cella con il numero di mine vicine. In realtà crea una copia della cella con i valori aggiornati
      cell.copy(adjacentMines = mineCount)
    }
  }
}

// Funzione per ottenere le celle adiacenti
def getAdjacentCells(grid: List[List[Cell]], row: Int, col: Int): List[Cell] = {
  val adjacentPositions = for {
    r <- (row - 1) to (row + 1)
    c <- (col - 1) to (col + 1)
    if r >= 0 && c >= 0 && r < grid.size && c < grid.head.size
    if !(r == row && c == col) // Evita di includere la cella stessa
  } yield grid(r)(c)

  adjacentPositions.toList
}

// Funzione per renderizzare la griglia di celle
def renderGrid(grid: List[List[Cell]]): Div = {
  div(
    grid.zipWithIndex.map { case (row, rowIndex) =>
      div(
        cls := "row",
        row.zipWithIndex.map {
          case (cell, colIndex) =>
            renderCell(grid, rowIndex, colIndex, cell)
        }
      )
    }
  )
}

// Funzione per renderizzare una singola cella
def renderCell(grid: List[List[Cell]], row: Int, col: Int, cell: Cell): Div = {
  div(
    cls := "cell",
    child.text <-- cell.revealed.signal.combineWith(cell.flagged.signal).map {
      case (revealed, flagged) =>
        if (flagged && !revealed) {
          "🚩"                               // Mostra una bandiera se è segnalata ma non ancora rivelata
        } else
          if (revealed) {
            if (cell.hasMine) {
              "💣"                           // Mostra una mina se la cella è rivelata e contiene una mina
            } else
            {
              if (cell.adjacentMines > 0)
                cell.adjacentMines.toString   // Mostra il numero di mine adiacenti
              else
                "0"                           // Mostra 0 se non ci sono mine adiacenti
            }
          } else
          {
            ""  // Lascia la cella vuota se non è né flaggata né rivelata
          }
    },
    onClick --> { _ =>
      if (!cell.revealed.now()) {
        revealCell(grid, row, col)
      }
    },
    onContextMenu.preventDefault.map(_ => ()) --> { _ =>
      if (!cell.revealed.now()) {
        cell.flagged.update(!_ )   // Alterna lo stato della bandiera
      }
    }
  )
}

// Funzione per rivelare una cella e gestire la logica di vittoria/sconfitta
def revealCell(grid: List[List[Cell]], row: Int, col: Int): Unit = {
  val cell = grid(row)(col)

  if (!cell.revealed.now() && !gameOver.now()) {
    cell.revealed.set(true)

    if (cell.hasMine) {
      // Se clicchiamo su una mina, perdiamo la partita
      gameOver.set(true)
      revealAllMines(grid)  // Funzione per rivelare tutte le mine
    } else if (cell.adjacentMines == 0) {
      revealAdjacentCells(grid, row, col, true)
    }

    // Controlla se tutte le celle senza mine sono state rivelate
    checkVictory(grid)
  }
}

// Funzione per rivelare tutte le mine in caso di sconfitta
def revealAllMines(grid: List[List[Cell]]): Unit = {
  grid.flatten.foreach { cell =>
    if (cell.hasMine) {
      cell.revealed.set(true)
    }
  }
}

// Funzione ricorsiva per rivelare le celle adiacenti sicure
def revealAdjacentCells(grid: List[List[Cell]], row: Int, col: Int, First: Boolean): Unit = {
  val currentCell = grid(row)(col)

  // Se la cella è già stata rivelata o ha una mina, non fare nulla
  if (!First)
    if (currentCell.revealed.now() || currentCell.hasMine) return
  //if (currentCell.hasMine) return

  // Rivela la cella
  currentCell.revealed.set(true)

  // Se la cella ha mine adiacenti, fermati
  if (currentCell.adjacentMines > 0) return

  // Altrimenti, continua a rivelare le celle adiacenti
  val adjacentPositions = for {
    r <- (row - 1) to (row + 1)
    c <- (col - 1) to (col + 1)
    if r >= 0 && c >= 0 && r < grid.size && c < grid.head.size
    if !(r == row && c == col) // Escludi la cella stessa
  } yield (r, c)

  adjacentPositions.foreach { case (r, c) =>
    revealAdjacentCells(grid, r, c, false) // Richiama la funzione ricorsivamente con le coordinate
  }
}

// Funzione per controllare la vittoria
def checkVictory(grid: List[List[Cell]]): Unit = {
  val allCells = grid.flatten
  val cellsRevealed = allCells.count(cell => cell.revealed.now())
  val nonMineCells = allCells.count(cell => !cell.hasMine)

  if (cellsRevealed == nonMineCells && !gameOver.now()) {
    gameOver.set(true)
  }
}

// Funzione principale che crea il layout e inizia il gioco
def main(): Div = {
  val gridSize = 8
  val numMines = 10
  val grid = createGrid(gridSize, numMines)

  div(
    h1("Campo Minato"),
    renderGrid(grid),  // Renderizza la griglia
    child.text <-- gameOver.signal.map { over =>
      if (over) {
        // Mostra un messaggio di vittoria o sconfitta
        if (grid.flatten.exists(cell => cell.hasMine && cell.revealed.now())) {
            "Hai perso! Riprova."
        } else {
          "Complimenti, hai vinto!"
        }
      } else {
        ""
      }
    }
  )
}

object Main {
  def main(args: Array[String]): Unit = {
    // Renderizza il gioco all'interno del body dell'HTML
    render(dom.document.body, CampoMinato.main())
  }
}
