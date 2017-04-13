import org.scalatest._
import playfair._
import playfair.Coder
import playfair.CoderPair
/** Class for unit test. 
 *  The test follow the specifications.
 *  
 *  @author Salvatore Rapisarda
 */
class PlayfairSpec extends FunSpec {
	val _playfair_test = new Coder 
	
	describe("create array key used for Encoding and Decoding"){
		
		it ("shoul be a function that accepts a string \"code\" and return and arrary not null."){ 
			val keycode ="Pennsylvania"
			val actual = _playfair_test.getPlayfairMaxtrixKeyCode(keycode)
			assert(actual.length > 0)
		}
		
		it ("shoul be a function that accepts a string \"code\" and return and array of 25 chars."){ 
			val keycode ="Pennsylvania"
			val actual = _playfair_test.getPlayfairMaxtrixKeyCode(keycode)
			assert(actual.length == 25 )
		}
		
		it ("shoul be a function that accepts a string \"code\" and return and array where all the chars are different."){ 
			val keycode ="Pennsylvania"
			val actual = _playfair_test.getPlayfairMaxtrixKeyCode(keycode)
			assert(actual.diff(actual.distinct).length == 0 )
		}
		
		it ("checking the exspected return code."){ 
			val keycode ="Pennsylvania"
			val actual = _playfair_test.getPlayfairMaxtrixKeyCode(keycode)
			val expected = "pensylvaibcdfghkmoqrtuwxz"
			assert(actual.mkString == expected )
		}
		
		it ("should return a key that should be and Array of 25 element (letter) 5 x 5") {
			val keycode ="salvatore"
			val actual = _playfair_test.getPlayfairMaxtrixKeyCode(keycode)
			val expected = 25
			assert(actual.length == expected )
		}
	
		it("should return a matrix that does not  contain just one letter j"){
			val keycode ="ajajajajaj"
			val actual = _playfair_test.getPlayfairMaxtrixKeyCode(keycode)
			assert(actual.mkString == "ajbcdefghiklmnopqrstuvwxy" )
			assert(actual.filter(p=>p=='j').length == 1)	
		} 
		
	}

	
	describe ("Encoding" ){
		it ("The text to be encriphered will be a single String of arbitrary text.") {
		  val text = "ordinary text"
		  assert( _playfair_test.encode(text) != Nil)
		}
		
		it ("The text will be mixed case, with spaces, newlines, and punctuation marks.") {
		  val text = "ordinary, text. @@£££ LLALAew23873872"
		  assert( _playfair_test.encode(text) != Nil)
		}
		
		describe ("The return value should be all lowercase; letters should be blocks of 5, with a single space between blocks (the last block may contain fewer than 5 characters)."){
			
			it ("he just returns the letters containd in abcdefghiklmnopqrstuvwxyz"){
				val text = "hallo my name is salvatore"
				val expected =   "halxlomynameissalvatorez"
				val actual =  _playfair_test.prepareToEncode(text)
				assert ( actual == expected  )  
			}
			
			
			
			
			it ("he return value should be all lowercase"){
			  val text = "Hallo Mynam eIssa lvato re"
			  val expected =   "halxl omyna meiss alvat orez"
			  val actual =  _playfair_test.makeBlocks(_playfair_test.prepareToEncode(text))
			  assert ( actual == expected  )  
			}
			
			it("Remove any punctuation or characters that are not present in the key square (this may mean spelling out numbers, punctuation etc.)"){ 
				val text = "aaaaaaa2aaaaaaa&&&aaa&&&aa&&&aaaaaa"
				val actual = _playfair_test.prepareToEncode(text) 
				assert (actual.matches("[a-z ]+") )
				
			} 
		
			it ("letters should be blocks of 5, with a single space between blocks"){
				val text = "Hallo my name is Salvatore"
			    val expected = "halxl omyna meiss alvat orez"
			    val actual =  _playfair_test.makeBlocks(_playfair_test.prepareToEncode(text))
			    assert ( actual == expected  )  
			}
			
			it ("There should be no whitespace at the beginning or the end, and only a single space ora single newline between blocks.") {
				val text = "  hallo my name is \n%%%  Salvatore :) "
				val expected = "halxl omyna meiss alvat orez"
			    val actual =  _playfair_test.makeBlocks(_playfair_test.prepareToEncode(text))
				assert ( actual == expected  ) 
			}
			
			it("If a final letter is needed to complete a pair, use 'z' "){
				val text = "hallomynameissalvatorer"
				val expected =  0  
				val actual =  _playfair_test.prepareToEncode(text).length() %  2
				assert(expected == actual ) 
			}	
			
			it ("he return a string of two characters"){
			  val text = "hi"
			  val expected =   "hi"
			  val actual =  _playfair_test.makeBlocks(text)
			  assert ( actual == expected  )  
			}
			
			it ("he return a string of two characters 'hj'"){
			  val text = "hj"
			  val expected =   "hj"
			  val actual =  _playfair_test.makeBlocks(text)
			  assert ( actual == expected  )  
			}
			
		}
				
		describe("If a double letter occurs in a pair, an 'x' will be inserted between them." ) {
			
			it("should be insert a 'x' between double letter "){
				val text = "hallomynameissalvatorera"
				val expected = "halxl omyna meiss alvat orera z"
				  
				val actual =  _playfair_test.makeBlocks(_playfair_test.prepareToEncode(text))
				assert(expected == actual )
			}
			
			it ( "should not insert any z at the end" ){
			  val text = "hallomynameissalvatorera"
			  val expected = "halxlomynameissalvatoreraz" 
			  val actual =  _playfair_test.prepareToEncode(text)
			  assert(expected == actual )
			}
		
			it ("Identify any double letters in the plaintext and replace the second occurence with an 'x' e.g. 'hammer' -> 'hamxmerz'.") {
			  val text = "hammer"
			  val expected = "hamxmerz" 
			  val actual =  _playfair_test.prepareToEncode(text)
			  assert(expected == actual )
			  
			}
			
			it("should change calling with calxling"){
			  val text = "calling"
			  val expected = "calxling" 
			  val actual =  _playfair_test.prepareToEncode(text)
			  assert(expected == actual )
			}
			
			it("If a pair is \"xx\", insert a 'q' between them."){
			  val text = "it's size is xxl"
			  val expected = "itsxsizeisxqxl" 
			  val actual =  _playfair_test.prepareToEncode(text)
			  assert(expected == actual )
			  
			}
			
		}
		
		it ( "If the plaintext has an odd number of characters, append an 'z' to the end to make it even.") {
			val text ="the sky is blu end is clear"
			val expected = "theskyisbluendisclearz" 
			val actual =  _playfair_test.prepareToEncode(text)
			assert(expected == actual )  
		}
		
		it ("should encrypt the  text in the coursework" ){
			val keycode = "Pennsylvania"
			val coder = new Coder(keycode)
			val text = "An anonymous reader sends word of a proof-of-concept Google Chrome browser " +
						"extension that steals users’ login details. The developer, Andreas Grech, " +
						"says that he is trying to raise awareness about security among end users, " + 
						"and therefore chose Chrome as a test-bed because of its reputation as the " +
						"safest browser."
			  
		  	val expected = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
							"dckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
							"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
							"bonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
							"dckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
							"kqxny m" 
		  	val actual =  coder.encode(text)
			assert(expected == actual )    
		}
		
		describe ("verify valid encoded"){
			it("should be a not valid  encoded string"){
			  val keycode = "Pennsylvania"
				val coder = new Coder(keycode)
				  
			  	val encoded = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
								"dckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
								"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
								"bons4n yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
								"dckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
								"kqxny m" 
			  
				val valid =  coder.isValidEncoded(encoded)
				assert(! valid )    
			  
			}
			
			it("should be a valid encoded string"){
			  val keycode = "Pennsylvania"
				val coder = new Coder(keycode)
				
				  
			  	val encoded = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
								"dckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
								"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
								"bonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
								"dckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
								"kqxny m" 
			  
				val valid =  coder.isValidEncoded(encoded)
				assert(valid )    
			  
			}
			
			it("should be a not valid  encoded string because a space more"){
			  val keycode = "Pennsylvania"
				val coder = new Coder(keycode)
				  
			  	val encoded = "fafaw aermw yqnvm  vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
								"dckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
								"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
								"bons4n yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
								"dckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
								"kqxny m" 
			  
				val valid =  coder.isValidEncoded(encoded)
				assert(! valid )    
			  
			}
			
			it("should be a not valid  encoded string because a spacial chars"){
			  val keycode = "Pennsylvania"
				val coder = new Coder(keycode)
				  
			  	val encoded = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
								"dckqu vhzwn ynmyz uns%g wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
								"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
								"bons4n yniar w$ynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
								"dckqu vinlw ny5lv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
								"kqxny m" 
			  
				val valid =  coder.isValidEncoded(encoded)
				assert(! valid )    
			  
			}
			
			it("should be a not valid  encoded string because nubers"){
			  val keycode = "Pennsylvania"
				val coder = new Coder(keycode)
				  
			  	val encoded = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
								"dckqu vhzwn ynmyz uns4g wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
								"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
								"bons4n yniar w5ynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
								"dckqu vinlw ny5lv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
								"kqxny m" 
			  
				val valid =  coder.isValidEncoded(encoded)
				assert(! valid )    
			  
			}
			
		}
		
		it ("should decode the  text in the coursework" ){
			val keycode = "Pennsylvania"
			val coder = new Coder(keycode)
			val expected =  "anano nymou sread ersen dswor dofap roofo fconc eptgo xogle\n"+ 
							"chrom ebrow serex tensi ontha tstea lsuse rslog indet ailst\n"+ 
							"hedev elope randr easgr echsa ystha theis tryin gtora iseaw\n"+ 
							"arene ssabo utsec urity among endus ersan dther efore chose\n"+ 
							"chrom easat estbe dbeca useof itsre putat ionas thesa festb\n"+
							"rowse r"
			  
		  	val text = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\n" +
							"dckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\n" +
							"dymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\n" +
							"bonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\n" +
							"dckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\n" +
							"kqxny m" 
		  
			val actual =  coder.decode(text)
			assert(expected == actual )    
		}
		
		describe ( "Wikipedia - Encrypting the message: Hide the gold in the tree stump"){
			val keycode = "playfair example"
			val playfairtest = new Coder(keycode)
			
			it ( "should prepare the string to encode like: hidethegoldinthetreestump"){
				val text ="Hide the gold in the tree stump"
				val expected = "hidethegoldinthetrexestump" 
				val actual =  playfairtest.prepareToEncode(text)
				assert(expected == actual )  
			}
			
			it( "the matrix code should be " ){
			  val actual = playfairtest.getPlayfairMaxtrixKeyCode(keycode).mkString
			  val expected = "playfirexmbcdghknoqstuvwz"
			    
			}
			
			it ( "should split in Pair"){
			  val text ="xm"
			  val expected = List ( new  CoderPair('x', 'm') )
			  val actual = playfairtest.splitToCoderPair(text.toList)
			  assert(expected == actual)
			}
			
			
			it ( "should convert in list of charecter "){
			  val expected = List ( 'x', 'm' )
			  val actual = playfairtest.coderPairToList( List( new  CoderPair('x', 'm') ))
			  assert(expected == actual)
			}
			
			it ( "should convert in string the list of charecter "){
			  val expected = "xm"
			  val actual = playfairtest.coderPairToList( List( new  CoderPair('x', 'm') )).mkString
			  assert(expected == actual)
			}
			
			describe ("Encoding" ){
				describe ("should encode the box shape"){
					it ("The pair 'hi' forms a rectangle, replace it with 'bm'"){
					  	val text ="hi"
						val expected = "bm" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					}
					it ("The pair 'th' forms a rectangle, replace it with 'zb'"){
					  	val text ="th"
						val expected = "zb" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					}
					it ("The pair 'eg' forms a rectangle, replace it with 'xd'"){
					  	val text ="eg"
						val expected = "xd" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					}
					
					it ("The pair 'eq' forms a rectangle, replace it with 'xo'"){
					  	val text ="eq"
						val expected = "xo" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					}
					
				  }
			
				describe ("should encode the cloumn shape"){
				  it ("should change the pair 'de' to 'od'"){
				    val text ="de"
					val expected = "od" 
					val actual =  playfairtest.encode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'av' to 'ea'"){
				    val text ="av"
					val expected = "ea" 
					val actual =  playfairtest.encode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'ik' to 'bt'"){
				    val text ="ik"
					val expected = "bt" 
					val actual =  playfairtest.encode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'ln' to 'ru'"){
				    val text ="ln"
					val expected = "ru" 
					val actual =  playfairtest.encode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'fz' to 'mf'"){
				    val text ="fz"
					val expected = "mf" 
					val actual =  playfairtest.encode(text)
					assert(expected == actual ) 
				  }
		
				}
			
				describe ("should encode the row shape"){
					
					it( "The pair 'ex' is in a row, replace it with 'xm'"){
					  val text ="ex"
						val expected = "xm" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					  
					}
					
				   it( "The pair 'xm' is in a row, replace it with 'mi'"){
					  val text ="xm"
						val expected = "mi" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					  
					}
	
					it( "The pair 'ks' is in a row, replace it with 'nk'"){
					  val text ="ks"
						val expected = "nk" 
						val actual =  playfairtest.encode(text)
						assert(expected == actual ) 
					  
					}
				}
			}

			describe ("Decoding" ){
				
			  describe ("should dencode the row shape "){
					it( "The pair 'xm' is in a row, replace it with 'ex'"){
					  val text ="xm"
						val expected = "ex" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					  
					}
					
				   it( "The pair 'mi'  is in a row, replace it with 'xm'"){
					  val text ="mi"
						val expected = "xm" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					  
					}
	
					it( "The pair 'nk' is in a row, replace it with 'ks'"){
					  val text ="nk"
						val expected = "ks" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					  
					}
					
					it( "The pair 'pa' is in a row, replace it with 'fl'"){
					  val text ="pa"
						val expected = "fl" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					  
					}
					
					it( "The pair 'oq' is in a row, replace it with 'no'"){
					  val text ="oq"
						val expected = "no" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					  
					}
					
				}
			  
			  
			  	describe ("should dencode the cloumn shape"){
				  it ("should change the pair 'od' to 'de'"){
				    val text ="od"
					val expected = "de" 
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'ea' to 'av'"){
				    val text ="ea"
					val expected =  "av"
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'bt' to 'ik'"){
				    val text ="bt" 
					val expected = "ik"
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'ru' to 'ln' "){
				    val text ="ru" 
					val expected = "ln"
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }
				  
				  it ("should change the pair 'mf' to 'fz'"){
				    val text ="mf"
					val expected = "fz" 
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }
				  it ("should change the pair 'pi' to 'tp'"){
				    val text ="pi"
					val expected = "tp" 
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }	
				  it ("should change the pair 'lr' to 'ul'"){
				    val text ="lr"
					val expected = "ul" 
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }	
				  it ("should change the pair 'xw' to 'yq'"){
				    val text ="lr"
					val expected = "ul" 
					val actual =  playfairtest.decode(text)
					assert(expected == actual ) 
				  }	
			  	}
				describe ("should decode the box shape"){
					it ("The pair 'bm' forms a rectangle, replace it with 'hi' "){
					  	val text = "bm"
						val expected =  "hi"
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					}
					it ("The pair 'zb' forms a rectangle, replace it with 'th' "){
					  	val text ="th"
						val expected = "zb" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					}
					it ("The pair 'xd forms a rectangle, replace it with 'eg''"){
					  	val text = "xd"
						val expected ="eg"  
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					}
					
					it ("The pair 'xo' forms a rectangle, replace it with 'eq'"){
					  	val text ="xo"
						val expected = "eq" 
						val actual =  playfairtest.decode(text)
						assert(expected == actual ) 
					}
				  
				}
			  
				it ("The text to be decoded will be in the format produced by the encoder.") {
					val textEncoded = "bmodz bxdna bekud muixm mouvi f"
					val expected = "hidet hegol dinth etrex estum p" 
					val actual  =   playfairtest.decode(textEncoded)
					assert(expected==actual)
					  
				}
			} 
					
			
			it ("Should encrypt the message: should Hide the gold in the tree stump"){
				val text ="Hide the gold in the tree stump"
				val expected = "bmodz bxdna bekud muixm mouvi f" 
				val actual =  playfairtest.encode(text)
				assert(expected == actual ) 
			}
			
			
			
		}
		
		
	} 
	
	
	
}

