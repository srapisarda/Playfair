package playfair


import java.nio.file.Paths
import java.nio.file.Files

import scala.io.Source
	
	
object Playfair {
   /** This is the main for running Playfair 
	* 
	*	1. It will ask whether to encode (1) , decode (2), or quit (0);
	*	2. then (unless quitting using 0) it will ask for the keyword,
	*	3. then it will ask for the name of a file to be encoded/decoded. 4. It will read in the file,
	*	5. encode or decode it, and
	*	6. display the result.
	* 	
	*  	@author Salvatore Rapisarda 
	**/
	def main(args: Array[String]) = {
			var opt = "0"
		    do  {
		    	printMenu
					println( "Please make your choise: ")
		    	opt = scala.io.StdIn.readLine().trim()
		    	printEncodeDecode(opt)
		    }while(opt != "0")
		    println("bye")		
	}
		      
		      
	private def printStars(howMany:Int):Unit= {
	 	 for ( i <- 1 to howMany) {
	 	 	print("*")
	 	 }
	 	 println()
	}                                                
	
	private def  printMenu = {
	    println()
	 	printStars(15)
	    println("Playfair Menu")
	 	printStars(15)
	 	println("1 - Encode")
	 	println("2 - Decode")	
	 	printStars(15)
	 	println("0 - Exit")
	}
		
	private def isValidKeyWord (keyCode:String )= 
	  keyCode.filterNot( p => ('a' to 'z').contains (p) ).length() <= 0 
	  
	 
	  
	private def getFileAsString(): (Boolean, String ) =  {
	   println()
		 println("Please enter your file name: ")
	   val fileName = scala.io.StdIn.readLine()
	   if ( Files.exists(Paths.get(fileName)) ){
		    (true,  Source.fromFile(fileName).getLines.toList.mkString)
	   }else (false , "") 
	}  
	
	
	private def printEncodeDecode ( encode:String) = {
	    if ( encode == "1" || encode == "2" ) {
	    	println()
				println("Please enter a key code: ")
	    	val keyCode = scala.io.StdIn.readLine()
	    	val coder = new Coder(keyCode)  
			println()
			val f =  getFileAsString
			if ( f._1 ){
				if ( encode == "1") 
				  println( coder.encode(f._2 ) )
				else if ( coder.isValidEncoded(f._2)) 
				  println( coder.decode(f._2) )
				else println("The file has not a valid encoding!!")   
			}else {
				println ( "The file inserted cannot be found!!!")
			}
		 	
	    } else println("Please  1 2 or 0 are the only valid options!!!")
	  
	}
			
			
		
	
}
	
	
	
