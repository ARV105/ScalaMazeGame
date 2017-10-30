/**
 * Created by Auriel on 1/24/2015.
 *
 * created a maze with the shortest possible path
 */
import swing._
import java.awt.Color
import java.awt.geom._

/* Array makes the maze, -1 is a wall, 0 is white space, can change it to try different mazes */
val maze=Array( Array( 0, -1, 0,  0,  0,  0,  0,  0, 0),
                Array( 0, -1, 0, -1,  0, -1,  0, -1, 0),
                Array( 0, -1, 0, -1,  0, -1,  0, -1, 0),
                Array( 0, -1, 0, -1,  0, -1, -1, -1, 0),
                Array( 0,  0, 0,  0,  0, -1,  0,  0, 0),
                Array( 0, -1, 0, -1, -1, -1,  0,  0, 0),
                Array( 0, -1, 0, -1,  0, -1, -1, -1, 0),
                Array( 0, -1, 0,  0,  0, -1,  0,  0, 0),
                Array( 0, -1,-1, -1,  0, -1,  0, -1,-1),
                Array( 0, 0, 0,  0,  0, -1,  0,  0, 0))


/* draws the maze */
val panel = new Panel {
  override def paint(g:Graphics2D): Unit = {
    for( i <- maze.indices; j <- maze(i).indices) {
      g.setPaint(if(maze(i)(j)==0) Color.white else
         if(maze(i)(j) == -1) Color.black else
         if(maze(i)(j) > 0) new Color(20+5*maze(i)(j) min 255,0,0) else
           Color.red)
      g.fill(new Rectangle2D.Double(i*size.width/maze.length,
        j*size.height/maze(i).length,size.width/maze.length,
      size.height/maze(i).length))
    }
  }
  preferredSize = new Dimension(500,500)
}
 /* algorithm fo find the path thru maze through one step back and forth */
def shortestPath(maze:Array[Array[Int]], x:Int, y:Int, ex:Int, ey:Int):Int = {
  if (x == ex && y == ey) 0
  else if (x < 0 || x >= maze.length || y > maze(x).length || maze(x)(y) < 0) {
    1000000000 // Not -1 or Int.MaxValue
  } else {
    maze(x)(y) = -2
    panel.repaint
    Thread.sleep(200)
    val ret = (shortestPath(maze, x + 1, y, ex, ey) min
      shortestPath(maze, x - 1, y, ex, ey) min
      shortestPath(maze, y + 1, x, ex, ey) min
      shortestPath(maze, y - 1, x, ex, ey)) + 1
    maze(x)(y) = 0
    ret

  }
}
/* finds the shortest path through maze */
def shortestPath2(maze:Array[Array[Int]], x:Int, y:Int, ex:Int, ey:Int, steps:Int):Int = {
  if (x == ex && y == ey) 0
  else if (x < 0 || x >= maze.length || y > maze(x).length || maze(x)(y) < 0) {
    1000000000 // Not -1 or Int.MaxValue
  }else if (maze(x)(y)>0 && maze(x)(y)<=steps) {
    1000000000
  } else {
    maze(x)(y) = steps
    panel.repaint
    Thread.sleep(200)
    val ret = (shortestPath2(maze, x + 1, y, ex, ey, steps+1) min
      shortestPath2(maze, x - 1, y, ex, ey, steps+1) min
      shortestPath2(maze, y + 1, x, ex, ey, steps+1) min
      shortestPath2(maze, y - 1, x, ex, ey, steps+1)) + 1
    ret

  }
}

val frame = new MainFrame {
  title = "Blood Maze"
  contents = panel
  centerOnScreen
}
frame.open
println(shortestPath2(maze,0,0,9,9,1))

// numberOfStep = 100
// 2^numberOfSteps = 2^100 -10^30
// 10^21 seconds
// 1 year ~ 3*10^7 seconds
/* 3*10^13 years- takes longer than the age of the universe to figure out maze if walls are taken
taken out */
