import Matrix2x2.createZeroMatrix
case class Vector2d(x: Double = 0, y: Double = 0) {

  def +(otherVector: Vector2d): Vector2d = {
    Vector2d(x + otherVector.x, y + otherVector.y)
  }

  def -(otherVector: Vector2d): Vector2d = {
    Vector2d(x - otherVector.x, y - otherVector.y)
  }

}

object Vector2d {

  def x(xvalue: Double): Vector2d = {
    Vector2d(xvalue, 0)
  }

  def y(yvalue: Double): Vector2d = {
    Vector2d(0, yvalue)
  }
}

case class Matrix2x2(a11: Double, a12: Double, a21: Double, a22: Double) {

  def multiplyByVector(vector: Vector2d): Matrix2x2 = {
    createZeroMatrix()
  }

  def +(otherMatrix: Matrix2x2): Matrix2x2 = {
    Matrix2x2(
      a11 + otherMatrix.a11,
      a12 + otherMatrix.a12,
      a21 + otherMatrix.a21,
      a22 + otherMatrix.a22
    )
  }

  def -(otherMatrix: Matrix2x2): Matrix2x2 = {
    Matrix2x2(
      a11 - otherMatrix.a11,
      a12 - otherMatrix.a12,
      a21 - otherMatrix.a21,
      a22 - otherMatrix.a22
    )
  }

  def *(otherMatrix: Matrix2x2): Matrix2x2 = {
    Matrix2x2(
      a11 * otherMatrix.a11 + a12 * otherMatrix.a21,
      a11 * otherMatrix.a12 + a12 * otherMatrix.a22,
      a21 * otherMatrix.a11 + a22 * otherMatrix.a21,
      a21 * otherMatrix.a12 + a22 * otherMatrix.a22
    )
  }

  def *(vec: Vector2d): Vector2d = {
    Vector2d(a11 * vec.x + a12 * vec.y, a21 * vec.x + a22 * vec.y)
  }

}

object Matrix2x2 {
  def createZeroMatrix(): Matrix2x2 = {
    Matrix2x2(0, 0, 0, 0)
  }

  def createUnitMatrix(): Matrix2x2 = {
    Matrix2x2(1, 0, 0, 1)
  }

  def rotation(radians: Double): Matrix2x2 = {
    Matrix2x2(
      math.cos(radians),
      -math.sin(radians),
      math.sin(radians),
      math.cos(radians)
    )
  }

}

@main def hello: Unit = {
  println("Hello world!")

  val vec1 = Vector2d(10.0, 20.0)
  val vec2 = Vector2d(-5, 3)

  val addVecRes = vec1 + vec2;
  val subtractVecRes = vec1 - vec2;
  println(s"vec1: $vec1")
  println(s"vec2: $vec2")
  println(s"addVecRes: $addVecRes")
  println(s"subtractVecRes: $subtractVecRes")

  val x = Vector2d.x(1.5)
  println(s"xVector: $x")
  val rot45 = Matrix2x2.rotation(math.Pi / 4)
  println(s"rot45 = $rot45")

  val res = rot45 * (rot45 * x)
  println(s"res = $res")

  val singleRotation = Matrix2x2.rotation(math.Pi / 2) * (x)
  println(s"singleRotation: $singleRotation")

}
// exercise password: zimno
