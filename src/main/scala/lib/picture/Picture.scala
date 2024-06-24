/**
 * Author: drs
 * Picture library
 * Version 2.1 2023
 */
package lib.picture
class Picture(cells: Seq[Seq[Char]]) {
  import Picture.*

  /**
   * A picture has depth (the number of rows)
   */
  val depth: Int = cells.length

  /**
   * A picture has width (the number of columns)
   */
  val width: Int = depth match
    case 0 => 0
    case _ => (cells map (_.length)).max

  /**
   * The characters are stored in a normalised format as text. When an
   * instance of Picture is created, the supplied parameter may have
   * ragged lines. The normalisation process makes a proper rectangle.
   */
  val text: Vector[Vector[Char]] = width match
    case 0 => Vector()
    case _ => cells.toVector map (_.padTo(width, Picture.space).toVector)

  /**
   * Check if the picture is empty.
   *
   * @return true if the picture has no dimension
   */
  def isEmpty: Boolean = depth == 0

  /**
   * It is useful to have the inverted version of isEmpty.
   *
   * @return true if the picture is non-empty.
   */
  def notEmpty: Boolean = !isEmpty

  /**
   * To display a picture, the data structure (Vector[Vector[Char]]) needs
   * to be compressed into a String, with newline characters inserted at
   * the ends of each row. This will display a picture nicely on an output
   * terminal.
   *
   * @return A String representation of a picture.
   */
  override def toString: String =
    text.map(_.foldRight("")(_ + _)).mkString("\n")

  /**
   * Transforms a picture by applying a function to each of its characters.
   *
   * @param f The function to transform individual characters.
   * @return A new picture representing the transformation.
   */
  def map(f: Char => Char): Picture =
    Picture(text.map(_.map(f)))

  /**
   * Above (aligned). Puts one picture above another. For this to work
   * properly, the two pictures must have the same width.  The only
   * exception is the empty picture which acts as a unit for this
   * operation.
   *
   * @param that The picture to be placed beneath this.
   * @return The composed picture: this above that.
   */
  private def /^/(that: Picture): Picture =
    if this.isEmpty then
      that
    else if that.isEmpty then
      this
    else
      //require(this.width == that.width, "/^/ non-conformant pictures")
      Picture(this.text ++ that.text)

  /**
   * Beside (aligned). Puts one picture beside another. For this to work
   * properly, the two pictures must have the same height.  The only
   * exception is the empty picture which acts as a unit for this
   * operation.
   *
   * @param that The picture to be placed to the right of this.
   * @return The composed picture: this beside that.
   */
  private def |+|(that: Picture): Picture =
    if this.isEmpty then
      that
    else if that.isEmpty
      then this
    else
      //require(this.depth == that.depth, "|+| non-conformant pictures")
      Picture((this.text zip that.text) map (_ ++ _))

  /**
   * Pads or truncates the width of a picture to the given new width.
   *
   * @param newWidth The required width of the new picture.
   * @param position Where the original is placed within a larger (padded)
   *                 picture. This is given as a percentage (from the
   *                 left margin).  Default is 0. This means that the
   *                 picture will be left-aligned and all padding is
   *                 placed to its right.
   * @param fill     The character to use for padding. Default is a space.
   * @return The new picture of the specified width.
   */
  def fixWidth(newWidth: Int, position: Int = LFT, fill: Char = space): Picture =
    val pos: Int = Integer.min(Integer.max(position, 0), 100)
    val len: Int = Math.abs(newWidth - this.width)
    val leftWidth: Int = len * pos / 100
    val rightWidth: Int = len - leftWidth
    if newWidth < 1 then
      empty
    else if (newWidth > width) then
      box(depth, leftWidth, fill) |+| this |+| box(depth, rightWidth, fill)
    else
      Picture(text map (_.slice(leftWidth, newWidth + leftWidth)))

  /**
   * Pads or truncates the depth of a picture to the given new depth.
   *
   * @param newWidth The required depth of the new picture.
   * @param position Where the original is placed within a larger (padded)
   *                 picture. This is given as a percentage (from the
   *                 bottom margin).  Default is 100. This means that the
   *                 picture will be top-aligned and all padding is
   *                 placed beneath.
   * @param fill     The character to use for padding. Default is a space.
   * @return The new picture of the specified depth.
   */
  def fixDepth(newDepth: Int, position: Int = TOP, fill: Char = space): Picture =
    val pos: Int = Integer.min(Integer.max(position, 0), 100)
    val len: Int = Math.abs(newDepth - depth)
    val topDepth: Int = len * pos / 100
    val botDepth: Int = len - topDepth
    if newDepth < 1 then
      empty
    else if newDepth > depth then
      box(topDepth, width, fill) /^/ this /^/ box(botDepth, width, fill)
    else
      Picture(text.slice((topDepth), (newDepth + topDepth)))

  /**
   * Above. Places two pictures above one another. If the pictures have
   * different widths, then the smaller picture is widened to fit, and
   * padding is inserted.
   *
   * @param that     The picture to place beneath this.
   * @param position The alignment of the smaller picture. Default is LFT.
   * @param fill     The padding character.
   * @return The new picture with this above that.
   */
  def ^(that: Picture, position: Int = LFT, fill: Char = space): Picture =
    if this.isEmpty then
      that
    else if that.isEmpty then
      this
    else if this.width < that.width then
      this.fixWidth(that.width, position, fill) /^/ that
    else
      this /^/ that.fixWidth(this.width, position, fill)

  /**
   * Beside. Places two pictures beside one another. If the pictures have
   * different depths, then the smaller picture is stretched to fit, and
   * padding is inserted.
   *
   * @param that     The picture to place to the right of this.
   * @param position The alignment of the smaller picture. Default is TOP.
   * @param fill     The padding character.
   * @return The new picture with this beside that.
   */
  def +(that: Picture, position: Int = TOP, fill: Char = space): Picture =
    if this.isEmpty then
      that
    else if that.isEmpty then
      this
    else if this.depth < that.depth then
      this.fixDepth(that.depth, position, fill) |+| that
    else
      this |+| that.fixDepth(this.depth, position, fill)

  /**
   * Above. A synonym for \^^ with its defaults
   * @param that The picture to be placed beneath this.
   * @return The new picture with this above that.
   */
  def above(that: Picture): Picture = this ^ that

  /**
   * Beside. A synonym for + with its defaults
   *
   * @param that The picture to be placed to the right of this.
   * @return The new picture with this beside that.
   */
  def beside(that: Picture): Picture = this + that

  /**
   * Change rows for columns and columns for rows.
   *
   * @return The transposed picture.
   */
  def transpose: Picture = Picture(text.transpose)

  /**
   * Reflect the picture around an imaginary line placed
   * horizontally in the middle.
   *
   * @return The reflected picture.
   */
  def reflectH: Picture = Picture(text.reverse)

  /**
   * Reflect the picture around an imaginary line placed
   * virtically in the middle.
   *
   * @return The reflected picture.
   */
  def reflectV: Picture = Picture(text map (_.reverse))

  /**
   * Rotates the picture clockwise by multiples of 90 degrees.
   *
   * @param quadrants The number of rotations. The parameter is
   *                  bound by modulo four, and the remaining
   *                  number determines the rotation:
   *                  0 = no rotation; 1 = 90 degrees;
   *                  2 = 180 degrees; 3 = 270 degrees.
   * @return
   */
  def rotate(quadrants: Int): Picture = quadrants % 4 match
    case 1 => this.transpose.reflectV
    case 2 => this.reflectH.reflectV
    case 3 => this.transpose.reflectH
    case _ => this

  /**
   * Place a border on the left hand side of the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def borderL(fill: Char = space): Picture = box(depth, 1, fill) + this

  /**
   * Place a border on the right hand side of the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def borderR(fill: Char = space): Picture = this + box(depth, 1, fill)

  /**
   * Place a border on the top of the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def borderT(fill: Char = space): Picture = box(1, width, fill) ^ this

  /**
   * Place a border on the bottom of the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def borderB(fill: Char = space): Picture = this ^ box(1, width, fill)

  /**
   * Place a border either side of the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def borderLR(fill: Char = space): Picture = this.borderL(fill).borderR(fill)

  /**
   * Place a border on the top and bottom of the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def borderTB(fill: Char = space): Picture = this.borderT(fill).borderB(fill)

  /**
   * Place a border all around the picture.
   *
   * @param fill The padding character. Default is a space.
   * @return The new picture with the border.
   */
  def border(fill: Char = space): Picture =
    this.borderT(fill).borderB(fill).borderL(fill).borderR(fill)

  /**
   * Place a frame (|) on the left hand side of the picture.
   *
   * @return The new picture with the frame.
   */
  def frameL: Picture = this.borderL(vert)

  /**
   * Place a frame (|) on the right hand side of the picture.
   *
   * @return The new picture with the frame.
   */
  def frameR: Picture = this.borderR(vert)

  /**
   * Place a frame (-) on the top of the picture.
   *
   * @return The new picture with the frame.
   */
  def frameT: Picture = this.borderT(horiz)

  /**
   * Place a frame (-) on the bottom of the picture.
   *
   * @return The new picture with the frame.
   */
  def frameB: Picture = this.borderB(horiz)

  /**
   * Place a frame (|) on left and right.
   *
   * @return The new picture with the frame.
   */
  def frameLR: Picture = this.frameL.frameR

  /**
   * Place a frame (-) on top and bottom.
   *
   * @return The new picture with the frame.
   */

  def frameTB: Picture = this.frameT.frameB

  /**
   * Place a frame (|, -) around the picture.
   *
   * @return The new picture with the frame.
   */
  def frame: Picture = this.frameL.frameR.frameT.frameB

  /**
   * Place a frame (|, -) around the picture but with no corners
   *
   * @return The new picture with the frame.
   */
  def frameNC: Picture =
    val newBorder = box(this.depth, 1, vert).fixDepth(this.depth + 2, CTR)
    newBorder + this.frameT.frameB + newBorder

  /**
   * Place a LR border of spaces and then frame with no corners
   * @return The new picture with the border and frame.
   */
  def frameBNC: Picture = this.borderLR().frameNC
    
} //class Picture

object Picture {
  private val space: Char = ' '
  private val horiz: Char = '-'
  private val vert: Char = '|'
  val TOP: Int = 0
  val MID: Int = 50
  val BOT: Int = 100
  val LFT: Int = 0
  val CTR: Int = 50
  val RGT: Int = 100
  
  def getSpace: Char = space
  def getHoriz: Char = horiz
  def getVert: Char = vert

  /**
   * Construct a new empty picture.
   *
   * @return The empty picture.
   */
  def apply(): Picture = new Picture(Vector())

  /**
   * A synonym for an empty picture.
   */
  val empty: Picture = apply()

  /**
   * Construct a new picture from a text string.
   *
   * @param text  The text will be split into lines.
   * @param delim The line delimeter. Default is \n.
   * @return The new picture with the given text.
   */
  def apply(text: String, delim: Char = '\n'): Picture =
    Picture(text.split(delim).toVector map (_.toVector))

  /**
   * Construct a new picture from a single character.
   *
   * @param c The character in the picture.
   * @return The new 1x1 picture.
   */
  def apply(c: Char): Picture = apply(c.toString)

  /**
   * Construct a new picture from a sequence of sequences
   * of Char.
   *
   * @param css The lines of characters.
   * @return The new picture.
   */
  def apply(css: Seq[Seq[Char]]): Picture = new Picture(css)

  /**
   * Create a solid box picture.
   *
   * @param d The depth of the picture.
   * @param w The width of the picture.
   * @param c The fill character.
   * @return The new picture.
   */
  def box(d: Int, w: Int, c: Char): Picture =
    if d <= 0 || w <= 0 then
      empty
    else
      val line = (1 to w).toVector map (_ => c)
      val lines = (1 to d).toVector map (_ => line)
      Picture(lines)

  /**
   * Creates a picture by flowing text from a string into a rectangle of
   * given width. The words are identified by removing any whitespace
   * characters from around them. No word is split unless it is longer
   * than the specified width. The depth of the picture is no more than
   * necessary to flow all of the words into the area.
   *
   * @param width The width of the columm into which the words flow.
   * @param s     The string containing the words.
   * @return A Picture with the words reformatted.
   */
  def flow(width: Int, s: String): Picture =
    if width > 0 then {
      val words = s.split("\\s+").toList
      // Break up any words that are too long
      val processedWords: List[String] = words.flatMap(word =>
        if word.length <= width then
          List(word)
        else
          word.grouped(width)
      )

      def consume(wordList: List[String], lineBuilder: String): Vector[String] =
        wordList match
          case List() => if lineBuilder.isEmpty then Vector() else Vector(lineBuilder)
          case nextWord :: restOfWords =>
            val nextWordLen = nextWord.length
            val lenSoFar = lineBuilder.length
            if lenSoFar + nextWordLen == width then
            // word fits exactly
              (lineBuilder ++ nextWord) +: consume(restOfWords, "")
            else if lenSoFar + 1 + nextWordLen <= width then
            // there is room on current line
              consume(restOfWords, lineBuilder ++ nextWord ++ " ")
            else if nextWordLen == width then
            // use the next line
              lineBuilder +: consume(restOfWords, nextWord)
            else
              lineBuilder +: consume(restOfWords, nextWord :+ ' ')

      consume(processedWords, "").map(Picture(_)).stack()
    } else
      Picture()

  extension (pictures: Seq[Picture])

    /**
     * A distributed above operator.
     *
     * @param position The alignment. Default = LFT.
     * @param fill     The padding. Default = space.
     * @return The picture representing the stack of pictures.
     */
    def stack(position: Int = LFT, fill: Char = space): Picture =
      pictures.foldRight(empty)((p, q) => p ^ (q, position, fill))

    /**
     * A distributed beside operator.
     *
     * @param position The alignment. Default = TOP.
     * @param fill     The padding. Default = space.
     * @return The picture representing the spread of pictures.
     */
    def spread(position: Int = TOP, fill: Char = space): Picture =
      pictures.foldRight(empty)((p, q) => p + (q, position, fill))

    /**
     * Make all the pictures in the list the width of the widest picture.
     *
     * @param position The alignment. Default = LFT.
     * @param fill     The padding. Default = space.
     * @return The normalised sequence of pictures.
     */
    def normaliseWidth(position: Int = LFT, fill: Char = space): Seq[Picture] =
      val maxWidth = if (pictures.isEmpty) 0 else (pictures map (_.width)).max
      pictures map (_.fixWidth(maxWidth, position, fill))
    /**
     * Make all the pictures in the list the depth of the deepest picture.
     *
     * @param position The alignment. Default = TOP.
     * @param fill     The padding. Default = space.
     * @return The normalised sequence of pictures.
     */
    def normaliseDepth(position: Int = TOP, fill: Char = space): Seq[Picture] =
      val maxDepth = if (pictures.isEmpty) 0 else (pictures map (_.depth)).max
      pictures map (_.fixDepth(maxDepth, position, fill))

    /**
     * Insert a separator picture between elements in a sequence of pictures.
     * @param sep The value to be made into a separator picture.
     * @return The sequence with the separator interspersed.
     */
    def intersperse(sep: Picture): Seq[Picture] =
      if pictures.length < 2 then
        pictures
      else
        pictures.head +: pictures.tail.flatMap(Seq(sep,_))
  
  extension (array: Seq[Seq[Picture]])

    /**
     * Normalise a sequence of sequence of pictures (rows by cols) such that
     * each row is depth normalised, and each column is width normalised.
     *
     * @param widthPos The width alignment. Default = LFT.
     * @param depthPos The depth alignment. Default = TOP.
     * @param fill     The padding. Default = space.
     * @return The normalised sequence of pictures.
     */
    private def normalise(widthPos: Int = LFT, depthPos: Int = TOP, fill: Char = space): Seq[Seq[Picture]] =
      val normalisedRows = array map (_.normaliseDepth(depthPos, fill))
      val normalisedCols = normalisedRows.transpose map (_.normaliseWidth(widthPos, fill))
      normalisedCols.transpose
    /**
     * Transform the rows of pictures into a table.
     *
     * @param widthPos The width alignment. Default = LFT.
     * @param depthPos The depth alignment. Default = TOP.
     * @param fill     The padding. Default = space.
     * @return The table with borders and frames around the cells.
     */
    def formatAsTable(widthPos: Int = LFT, depthPos: Int = TOP, fill: Char = space): Picture =
      val padding = Picture(fill)
      val maxCols = if array.isEmpty then 0 else array.map(_.length).max
      val padded = for row <- array yield row ++ Seq.tabulate(maxCols - row.length)(_ => padding)
      val normalsed = padded.normalise(widthPos, depthPos, fill)
      val rows = normalsed.map(_.map(_.borderL().borderR().frameL).spread().frameR)
      val tab = rows.map(_.frameT).stack().frameB
      tab
} //object Picture
