
/** Provides classes for enciphering and deciphering messages, using Playfair.  
  * 
  * http://en.wikipedia.org/wiki/Playfair_cipher
  * 
  * ==Overview==
  * The main class to use is [[playfair.Coder]], as so
  * {{{
  * val coder = new Coder("playfair example")
  * val encoded =  coder.encode("Hide the gold in the tree stump")
  * println(encoded)
  * val decoded =  coder.decode(encoded)
  * println(decoded)
  * }}}
  * 
  * @author Salvatore Rapisarda
  */
package playfair


/** This class implements the Playfair Cipher for enciphering and deciphering  messages
 *  
 *  @constructor create a new Coder with a keyword that for default is an empty string.
 *  @param keyword is the keyword used to for enciphering and deciphering 
 */ 
class Coder (keyword: String = ""){
	private val dimension = 5
	
   /** This returns blocks of 5, with a single space between blocks 
	 *  (the last block may contain fewer than 5 characters).
	 * There will be ten blocks per line (the last line may have fewer blocks).
	 */
	def makeBlocks(plainText: String) :String  = {
		def makeBlocksLoop(l:List[Char], acc:List[Char], nChar:Int, nBlocks:Int ):List[Char] = l match{
	  	  case List() => acc
	  	  case x::xs => if (acc.length != 0 && nChar % 5 == 0){ 
	  		  				if ( nBlocks != 0 && nBlocks % 9 == 0  ) makeBlocksLoop( xs, acc:::List( '\n' , x), nChar + 1, 0 ) 
	  		  				else makeBlocksLoop( xs, acc:::List( ' ' , x), nChar + 1, nBlocks+1 ) 
	  	  				}else 
	  		  				makeBlocksLoop( xs,  acc:+x , nChar+1, nBlocks ) 
		}
		makeBlocksLoop( plainText.toList, List(), 0, 0 ).
			foldLeft("")((x,y) => x + y)
	}
	
  /** This function is used to convert a List of Char to  list of CoderPair
    * 
    * @param l List of Char  
    * @return a list of CoderPair
	*/
    def splitToCoderPair(l:List[Char]) : List[CoderPair] = {
		def split( l :List[Char], acc:List[CoderPair] ) : List[CoderPair] = l match {
		    case List() => acc 
		    case x::xs => if (xs == List() )  split ( List(), new CoderPair(x, 'z' ) :: acc )
		    			  else split ( xs.tail,  new CoderPair(x, xs.head ) :: acc)
		}
		split(l,List()).reverse
	}
	
  /** This function is used to convert a list of CoderPair to to List of Char  
    * 
    * @param l list of CoderPair to convert to List of Char 
    * @return a List of Char 
	*/
	def coderPairToList(  l: List[CoderPair] ): List[Char] = {
		
	    def toList( l :List[CoderPair], acc:List[Char] ) : List[Char] = l match {
		    case List() => acc 
		    case x::xs =>  toList ( xs,  x.p2 :: x.p1 :: acc   )
		}
		toList ( l, List()  ).reverse
	} 
	
   /** This is used to encode a function
	 * The return value of all lower-case; letters are blocks of 5, with a 
	 * single space between blocks (the last block may contain fewer than 5 characters).
	 * There will be ten blocks per line (the last line may have fewer blocks).
	 * There it not whitespace at the beginning or the end, and only a single space or a single newline between blocks
	 * All the punctuation should be discarded.
	 *  
	 * @param plainText is the text to be enciphered will be a single String of arbitrary text.
	 *		  The text will be mixed case, with spaces, newlines, and punctuation marks.
	 * @return the string encoded 
	 *    	  
	 */
	def encode(plainText: String): String = { 
	  val matrixKeyCode = getPlayfairMaxtrixKeyCode(this.keyword)
	  val coderPairs = splitToCoderPair(prepareToEncode(plainText).toList)
	  
	  def loopEncode( l: List[CoderPair], acc:List[CoderPair]  ) : List[CoderPair] = l match{
	    case List() => acc 
	    case x::xs => loopEncode(xs, getCoderPair(matrixKeyCode, x, true) :: acc)
	  }
	  
	  makeBlocks ( coderPairToList(loopEncode(coderPairs, List()).reverse).mkString  )
	}
	
   /** This is used to decode a plain text
	 * The result will have the same structure as the encoded text 
	 * (blocks of five letters, whitespace as specified above.
	 * The number of blocks, and the number of characters per block, 
	 * may not be the same as in the encoded file.
	 * It is checking if the text is encoded properly otherwise it returns an 
	 * empty string. 
	 * 
	 * @param The text to be decoded will be in the format produced by the encoder
	 * @return if the test is encoded it returns a string decoded, otherwise it returns 
	 * 		   an empty string.   
	 */
	def decode(text: String): String = { 
	  val matrixKeyCode = getPlayfairMaxtrixKeyCode(this.keyword)
	  val coderPairs = splitToCoderPair(text.toLowerCase.toList.filter(p=>allowedChar.contains(p)))
	  def loopEncode( l: List[CoderPair], acc:List[CoderPair]  ) : List[CoderPair] = l match{
	    case List() => acc 
	    case x::xs => loopEncode(xs, getCoderPair(matrixKeyCode, x, false)::acc)
	  }
	  if (isValidEncoded(text))
		  makeBlocks ( coderPairToList(loopEncode(coderPairs, List()).reverse).mkString  )
	  else ""	  
	}
	
	
	/**This verify that the string to decode is properly encoded 
	 * 
	 * @param encoded is the string to verify
	 * @return true if the string can is a valid encoded, otherwise false 
	 */
	def isValidEncoded(encoded:String):Boolean =
			(for { j <- encoded.replaceAll("(\r\n)|\r|\n" , " ").split(' ')
			if( !(  j.foldLeft(true)((x,y)=> x && ('a' to 'z').contains(y) )
						&& j.size <= 5))
		} yield j).size <= 0
	
   /** This is used by the getCoderPairInRow and getCoderPairInCol
	 * returns a new code pair during the process of encode or decode of a row or a column
	 *  
	 * @param matrixKeyCode Array of char that represent the key code for encode or decoding
	 * @param CoderPair is the pair of char to encode or decode
	 * @param getNextPos function to apply for getting the pair to encode or decode
	 * @return a list of CoderPair
	 */
	private val  getNextCodePair = ( matrixKeyCode:Array[Char], p:CoderPair, getNextPos: (Int) =>Int    ) => { 
	  new CoderPair( 
		    matrixKeyCode( getNextPos(matrixKeyCode.indexOf( p.p1)  )) , 
					matrixKeyCode( getNextPos(matrixKeyCode.indexOf( p.p2)  )) )
	}
	
   /** This is used to encode or decode a pair of characters that is
	 * defined in a row 
	 * 
	 * @param matrixKeyCode Array of char that represent the key code for encode or decoding
	 * @param pair is the CoderPair of char to encode or decode
	 * @param encode if is true encode otherwise decode
	 * @return a CoderPair that is the result of the encoding or decoding
	 */
	private def getCoderPairInRow(matrixKeyCode:Array[Char], p:CoderPair, encode:Boolean ):CoderPair = {
		getNextCodePair( matrixKeyCode, p, 
								(pos:Int ) => 	if ( encode){
								  					if (  (pos + 1) % dimension != 0  )  (pos+1) 
								  					else pos + 1 - dimension
												}else{
													if (  (pos) % dimension != 0  )  (pos-1) 
								  					else pos - 1 + dimension
												} )
											
	}
	
   /** This function is used to encode or decode a pair of characters that are  
	 * placed in a column
	 * 
	 *  @param  matrixKeyCode Array of char that represent the key code for encode or decoding
	 *  @param pair is the CoderPair of char to encode or decode
	 *  @param encode if is true encode otherwise decode
	 *  @return a CoderPair that is the result of the encoding or decoding
	 */
	private def getCoderPairInCol(matrixKeyCode:Array[Char], p:CoderPair, encode:Boolean ):CoderPair = {
	  getNextCodePair( matrixKeyCode, p,  (pos:Int ) => 
	  								if ( encode ){
	  									if ( pos + dimension < dimension*dimension) (pos+dimension) 
	  									else pos % dimension   
	  								}else {
	  									if ( pos - dimension >=  0 ) (pos-dimension) 
	  									else (dimension*dimension) - (dimension - (pos % dimension))   
	  								})
	  								
										
	}
	
   /** This is used to encode or decode a pair of characters that 
	 * define a square 
	 * (int, int) ---------------
	 * -						-
	 * -						-
	 * -						-
	 * -----------------(int, int)
	 * 
	 * 
	 * @param matrixKeyCode Array of char that represent the key code for encode or decoding
	 * @param coord1 is a pair of {{{ (int, int) }}} that represent the position of the char in the matrix
	 * @param coord1 is a pair of {{{ (int, int) }}} that represent the position of the char in the matrix 
	 * @return a CoderPair that is the result of the encoding or decoding 
	 */
	private def getCoderPairInSquare(matrixKeyCode:Array[Char], coord1:(Int, Int), coord2:(Int,Int)  ):CoderPair = {
		new CoderPair(  matrixKeyCode(  coord1._1 * dimension + coord2._2 ), 
						 matrixKeyCode(  coord2._1 * dimension + coord1._2 ))
	}	
	
   

	/** This is used to encode or decode the code pairs.
	 *  
	 *  @param  matrixKeyCode Array of char that represent the key code for encode or decoding
	 *  @param pair is the CoderPair of char to encode or decode
	 *  @param encode if is true encode otherwise decode 
	 */  		
	private def getCoderPair(matrixKeyCode:Array[Char], pair:CoderPair, encode:Boolean ) : CoderPair  = {
	  val coordinates = ( c:Char) => 
		  		 (matrixKeyCode.indexOf(c) / dimension, matrixKeyCode.indexOf(c) % dimension)
	  
	  val c1 = coordinates(pair.p1)
	  val c2 = coordinates(pair.p2)
	  // Same row
	  if (c1._1  == c2._1 )  getCoderPairInRow(matrixKeyCode, pair, encode )
	  // same column 
	  else if (c1._2  == c2._2 ) {
		 getCoderPairInCol(matrixKeyCode, pair, encode)
	  }
	  // it should be a square
	  else { 
	     getCoderPairInSquare ( matrixKeyCode, c1, c2 ) 
	  } 
	}
	
   /** This function is use to format and prepare the text before to be
	 * be encoded.
	 * 
	 * @param str is the string that 
	 * @return the string ready to encode
	 */
	def prepareToEncode(str: String):String = {
		def cleanString( l:List[Char], acc:List[Char] ):List[Char] = 
			l match {
				// If a final letter is needed to complete a pair, use ’z’.
				case List() => if (acc.length % 2 == 0 ) acc else 'z' ::  acc 
				case x::xs => {
					if ( allowedChar contains x ) { 
					  if ( (acc.length + 1) % 2 == 0 && acc.head == x  ){
					      // If a pair is "xx", insert a ’q’ between them.
						  if (x=='x') cleanString(xs, x :: 'q' ::  acc ) 
						  // If a double letter occurs in a pair, an ’x’ will be inserted between them.
						  else cleanString(xs,x :: 'x' :: acc )
					  }else cleanString(xs, x :: acc) 
					} else cleanString( xs, acc)
				}
			}
		
		cleanString( str.toLowerCase.toList, List() ).reverse.mkString 
	}
	
	/** This is a string that represent all the possible allowed characters 
	 *  that the plain text can have. It does not contains any 'j',
	 *  because any ’j’ in the input is replaced by ’i’.
	 */
	private val allowedChar = ('a' to 'z').filter(p=> p!= 'j')
			.foldRight("")((x,y) =>  x + String.valueOf(y) )
	
	/** This is used to create the key code matrix 
	 *  
	 *  @param keycode is the string used to encode or decode 
	 *  @return an array of char represent the key-code   
	 */					
	def getPlayfairMaxtrixKeyCode(keycode: String):Array[Char] = {
		
		def loopMakePlayfairCode( ls:List[Char], acc:List[Char] ): List[Char] = ls match {
			case List() => acc 
			case x::xs  => 	if (acc.length >= 25 ) acc 
							else if( acc.contains(Character.toLowerCase(x))  ) loopMakePlayfairCode( xs, acc ) 
							else  loopMakePlayfairCode( xs, Character.toLowerCase(x) :: acc )
							
		}  
		
		def claenKeyCode ( keyCode:String  ): String = 
			keyCode.map(Character.toLowerCase).filter( c => ('a' to 'z').contains(  c )  )
		
		
		loopMakePlayfairCode(claenKeyCode ( keycode ).concat(allowedChar).toList, List()).reverse.toArray
		
	}

}

/** This class is used to create pair of char
 *  in the Coder class
 *  @constructor create a new Coder with a keyword that for default is an empty string. 
 *  @param p1 is the fist char pair, p1 is the second char pair
 *  
 */
case class CoderPair(p1:Char, p2:Char)