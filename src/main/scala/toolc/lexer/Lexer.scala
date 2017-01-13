package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "program"  => Some(PROGRAM())
    case "class" => Some(CLASS())
    case "def" => Some(DEF())
    case "var" => Some(VAR())
    case "String" => Some(STRING())
    case "Int" => Some(INT())
    case "Bool" => Some(BOOLEAN())
    case "while" => Some(WHILE())
    case "extends" => Some(EXTENDS())
    case "if" => Some(IF())
    case "else" => Some(ELSE())
    case "return" => Some(RETURN())
    case "length" => Some(LENGTH())
    case "true" => Some(TRUE())
    case "false" => Some(FALSE())
    case "this" => Some(THIS())
    case "new" => Some(NEW())
    case "println" => Some(PRINTLN())
    case "do" => Some(DO())
    case _   => None
  }


  /** Reads the contents of a file, caching two characters at a time.
    * That way we can have a 2-character lookahead with
    * currentChar and nextChar
    */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /** Consumes a character from the input.
      * nextChar becomes currentChar,
      * nextChar points to the first unread character.
      */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
  }


  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    val reader = new SourceReader(f)
    import reader._
     //catch comment which is not closed at the end of the file
    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {
      while (Character.isWhitespace(currentChar)) {
        consume()
      }

      if (currentChar == '/' && nextChar == '/') {
        consume(2)
        while(currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        var end = false
        consume(2)
        while(!end){
          while((currentChar != '*' || nextChar != '/') && !end) {
            consume(1)
            if(currentChar == EndOfFile){
              end = true
              ctx.reporter.error("Block comment not closed")
            }
          }
          if(!end){
            consume(2)
            end = true
          }
        }
        nextToken()  			  
      } else {
        readToken()
      }
    }

    /** Reads the next token from the stream. */
    def readToken(): Token = {
      // The position at the beginning of the token.
      val tokenPos = currentPos
      var token: Token = null
      if(currentChar == EndOfFile){
        token = EOF()
      }else{
        currentChar match { //style
          case '!' => consume(1);token = BANG()
          case '*' => consume(1);token = TIMES()
          case '/' => consume(1);token = DIV()
          case '+' => consume(1);token = PLUS()
          case '-' => consume(1);token = MINUS()
          case '<' => consume(1);token = LESSTHAN()
          case ':' => consume(1);token = COLON()
          case ';' => consume(1);token = SEMICOLON()
          case '.' => consume(1);token = DOT()
          case ',' => consume(1);token = COMMA()
          case '(' => consume(1);token = LPAREN()
          case ')' => consume(1);token = RPAREN()
          case '[' => consume(1);token = LBRACKET()
          case ']' => consume(1);token = RBRACKET()
          case '{' => consume(1);token = LBRACE()
          case '}' => consume(1);token = RBRACE()

          case '&' =>
            consume(1)
            if(currentChar == '&') {
              consume(1)
              token = AND()
            } else {
              token = BAD()
              ctx.reporter.error("Single ampersand is not allowed")
            }
            
          case '|' =>
            consume(1)
            if(currentChar == '|') {
              consume(1)
              token = OR()
            } else {
              token = BAD()
              ctx.reporter.error("Single vertical bar is not allowed")
            }
          case '=' =>
            consume(1)
            if(currentChar == '='){
              consume(1)
              token = EQUALS()
            } else {
              token = EQSIGN()
            }
          case c if c.isLetter =>
            var ident = c.toString
            consume(1)
            while(currentChar.isLetterOrDigit || currentChar == '_') {
              ident = ident + currentChar
              consume(1)
            }
            if(keywords(ident).isEmpty == true){
              token = ID(ident)
            }else{
              token = keywords(ident).get
            }
          case x if (x.isDigit )  =>
            var integerlit = x.asDigit
            consume(1)
            var end = false
            while(!end) {
              if (currentChar.isDigit){
                integerlit = integerlit * 10 + currentChar.asDigit
                consume(1)
              }else{
                end = true
              }
            }
            token = INTLIT(integerlit)
          case '"' =>
              	var stringlit = ""
              	var end = false
              	consume(1)
              	while(currentChar != '"' && !end) {
              	  if(currentChar == '\n' || currentChar == EndOfFile) {
              	    end = true
              	  } else {
              		  stringlit = stringlit + currentChar
              	  }
              	  consume(1)
              	}
              	consume(1)
                if(end == true){
                  token = BAD()
                  ctx.reporter.error("Non Terminated String")
                }else{
                  token = STRINGLIT(stringlit)
                }
            case _ =>
              token = BAD()
              ctx.reporter.error("Bad character found")
              consume(1)
        }
      }
      token.setPos(tokenPos)
      token
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}

/** Reads and displays the tokens, then returns a fresh iterator with the same tokens. */
object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}
