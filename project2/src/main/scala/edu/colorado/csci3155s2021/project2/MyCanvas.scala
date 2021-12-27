package edu.colorado.csci3155s2021.project2

/* A class to maintain a canvas */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(x: Double, y: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */
case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    // This function returns a 4-tuple (xmin, xmax, ymin, ymax)
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        (cList.minBy(_._1)._1, cList.maxBy(_._1)._1, cList.minBy(_._2)._2, cList.maxBy(_._2)._2)
    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(x: Double, y: Double): Polygon = {
        Polygon(cList.map { case (a, b) => (a+x, b+y) })
    }
    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)

    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = (c._1 - r, c._1 + r, c._2 - r, c._2 + r)

    //TODO: Create a new circle by shifting the center
    override def translate(x: Double, y: Double): MyCircle = MyCircle((c._1 + x, c._2 + y), r)


    // Function: render -- draw the circle. Do not edit this function
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */

class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val list = listOfObjects.map(x => (x.getBoundingBox._1, x.getBoundingBox._3)) ++ listOfObjects.map(x => (x.getBoundingBox._2, x.getBoundingBox._4))
        (list.minBy(_._1)._1, list.maxBy(_._1)._1, list.minBy(_._2)._2, list.maxBy(_._2)._2)
    }
    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = new MyCanvas(listOfObjects.map(_.translate(shiftX, shiftY)))

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
    val canvas1 = this.getBoundingBox
    val canvas2 = myc2.getBoundingBox
    this.overlap(myc2.translate(canvas1._2 - canvas2._1, ((canvas1._4 - canvas1._3) / 2) - ((canvas2._4 - canvas2._3) / 2)))
    }
    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val canvas1 = this.getBoundingBox
        val canvas2 = myc2.getBoundingBox
        this.overlap(myc2.translate(((canvas1._2 - canvas1._1) / 2) - ((canvas2._2 - canvas2._1) / 2),canvas1._4 - canvas2._3))
    }
    //TODO: Write a function that will rotate each figure about the center of its bounding box in the canvas using
    // the angle `ang` defined in radians.
    // The writeup provided describes how to implement the rotation.
    // Hint: Write helper functions to rotate a Polygon and a circle. Then you can simply use
    // translation, followed by rotation of individual objects and translation back.
    def rotate(angRad: Double): MyCanvas = {
        val x = (this.getBoundingBox._1 + this.getBoundingBox._2 ) / 2
        val y = (this.getBoundingBox._3 + this.getBoundingBox._4 ) / 2
        val listObj = this.translate(-1*x,-1*y).listOfObjects.map{
            case circle: MyCircle => {
                MyCircle((circle.c._1*math.cos(angRad) - circle.c._2*math.sin(angRad), circle.c._1*math.sin(angRad) + circle.c._2*math.cos(angRad)),circle.r)
            }
            case polygonal: Polygon => {
                Polygon(polygonal.cList.map((pair: (Double,Double)) => (pair._1*math.cos(angRad) - pair._2*math.sin(angRad),pair._1*math.sin(angRad) + pair._2*math.cos(angRad))))
            }
        }
        new MyCanvas(listObj).translate(x,y)
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }

    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    // DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    // DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
