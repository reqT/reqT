package reqt

import org.fife.ui.rsyntaxtextarea.AbstractTokenMakerFactory
import org.fife.ui.rsyntaxtextarea.TokenMakerFactory
import org.fife.ui.rsyntaxtextarea.AbstractTokenMaker
import org.fife.ui.rsyntaxtextarea.TokenTypes
import org.fife.ui.rsyntaxtextarea.TokenMap
import org.fife.ui.rsyntaxtextarea.Token
import org.fife.ui.rsyntaxtextarea.RSyntaxUtilities
import javax.swing.text.Segment

//https://github.com/bobbylight/RSyntaxTextArea/wiki/Adding-Syntax-Highlighting-for-a-new-Language

object ReqTTokenMaker:
  val EntTokenType = TokenTypes.DATA_TYPE
  val IntAttrTokenType = TokenTypes.RESERVED_WORD
  val StrAttrTokenType = TokenTypes.RESERVED_WORD_2
  val RelTokenType = TokenTypes.FUNCTION
  val nbrOfTokens = meta.concepts.size
  val tokenMap = TokenMap(nbrOfTokens)
  for c <- meta.entityNames   do tokenMap.put(c, ReqTTokenMaker.EntTokenType)
  for c <- meta.intAttrNames  do tokenMap.put(c, ReqTTokenMaker.IntAttrTokenType)
  for c <- meta.strAttrNames  do tokenMap.put(c, ReqTTokenMaker.StrAttrTokenType)
  for c <- meta.relationNames do tokenMap.put(c, ReqTTokenMaker.RelTokenType)

  def init(): Unit =
    val tmf = TokenMakerFactory.getDefaultInstance().asInstanceOf[AbstractTokenMakerFactory]
    tmf.putMapping("text/reqT", "reqt.ReqTTokenMaker")

class ReqTTokenMaker extends AbstractTokenMaker:
  wordsToHighlight = ReqTTokenMaker.tokenMap

  def exists(p: Token => Boolean): Boolean = 
    var current: Token = firstToken
    var found = false
    while current != null && current.getType() != TokenTypes.NULL && !found do
      found = p(current)
      current = current.getNextToken()
    end while
    found

  def firstReqTToken(): Option[Token] = 
    var current: Token = firstToken
    
    def isReqTTokenType: Boolean = 
      val t = current.getType()
      t == ReqTTokenMaker.EntTokenType ||
      t == ReqTTokenMaker.IntAttrTokenType ||
      t == ReqTTokenMaker.StrAttrTokenType ||
      t == ReqTTokenMaker.RelTokenType

    while current != null && current.getType() != TokenTypes.NULL && !isReqTTokenType do
      current = current.getNextToken()
    end while
    Option(current)

  def hasStrAttr() = exists(_.getType == ReqTTokenMaker.StrAttrTokenType)
  def hasRelAttr() = exists(_.getType == ReqTTokenMaker.RelTokenType)

  override def getWordsToHighlight(): TokenMap = wordsToHighlight

  override def addToken(segment: Segment, start: Int, end: Int, tokenType: Int, startOffset: Int): Unit =
    // This assumes all keywords, etc. were parsed as "identifiers."
    var actualTokenType = tokenType
    if tokenType == TokenTypes.IDENTIFIER then // find special tokens to highlight
      val tt: Int = wordsToHighlight.get(segment, start, end)
      if tt != -1 then 
        def firstType: Int = 
          if firstReqTToken().isDefined then firstReqTToken().get.getType() 
          else TokenTypes.NULL

        import ReqTTokenMaker.*
        tt match 
        case EntTokenType     if !hasStrAttr() => actualTokenType = tt
        case IntAttrTokenType if !hasStrAttr() => actualTokenType = tt
        case StrAttrTokenType if !hasStrAttr() => actualTokenType = tt
        case RelTokenType if firstType == EntTokenType && !hasStrAttr() && !hasRelAttr() => 
          actualTokenType = tt
        case _ => () // no special token found; do nothing

    super.addToken(segment, start, end, actualTokenType, startOffset)
  end addToken

  /**
   * Returns a list of tokens representing the given text.
   *
   * @param text The text to break into tokens.
   * @param startTokenType The token with which to start tokenizing.
   * @param startOffset The offset at which the line of tokens begins.
   * @return A linked list of tokens representing <code>text</code>.
   */
  override def getTokenList(text: Segment, startTokenType: Int, startOffset: Int): Token = 
    resetTokenList()

    val array: Array[Char] = text.array;
    val offset: Int = text.offset;
    val count: Int = text.count;
    val end: Int = offset + count;

    // Token starting offsets are always of the form:
    // 'startOffset + (currentTokenStart-offset)', but since startOffset and
    // offset are constant, tokens' starting positions become:
    // 'newStartOffset+currentTokenStart'.
    val newStartOffset: Int = startOffset - offset;

    var currentTokenStart: Int = offset;
    var currentTokenType: Int  = startTokenType;

    var i = offset
    while i < end do
      val c = array(i)

      currentTokenType match
      case TokenTypes.NULL =>
        currentTokenStart = i;   // Starting a new token here

        c match 
        case ' ' | '\t' => currentTokenType = TokenTypes.WHITESPACE
        case '"' => currentTokenType = TokenTypes.LITERAL_STRING_DOUBLE_QUOTE
        case '#' => currentTokenType = TokenTypes.COMMENT_EOL
        case _ =>
          if RSyntaxUtilities.isDigit(c) then currentTokenType = TokenTypes.LITERAL_NUMBER_DECIMAL_INT
          else if RSyntaxUtilities.isLetter(c) || c == '/' || c=='_' then 
            currentTokenType = TokenTypes.IDENTIFIER
          else 
            currentTokenType = TokenTypes.IDENTIFIER

      case TokenTypes.WHITESPACE => 

        c match 
        case ' ' | '\t' => // Do nothing; Still whitespace
        case '"' => 
          addToken(text, currentTokenStart,i-1, TokenTypes.WHITESPACE, newStartOffset+currentTokenStart)
          currentTokenStart = i
          currentTokenType = TokenTypes.LITERAL_STRING_DOUBLE_QUOTE
        case '#' => 
          addToken(text, currentTokenStart,i-1, TokenTypes.WHITESPACE, newStartOffset+currentTokenStart)
          currentTokenStart = i
          currentTokenType = TokenTypes.COMMENT_EOL
        case _ =>   // Else add the whitespace token and start anew.
          addToken(text, currentTokenStart,i-1, TokenTypes.WHITESPACE, newStartOffset+currentTokenStart)
          currentTokenStart = i
          if RSyntaxUtilities.isDigit(c) then
            currentTokenType = TokenTypes.LITERAL_NUMBER_DECIMAL_INT
          else if RSyntaxUtilities.isLetter(c) || c=='/' || c=='_' then
            currentTokenType = TokenTypes.IDENTIFIER
          else 
            currentTokenType = TokenTypes.IDENTIFIER

      case TokenTypes.IDENTIFIER =>

        c match 
        case ' ' | '\t' =>
          addToken(text, currentTokenStart,i-1, TokenTypes.IDENTIFIER, newStartOffset+currentTokenStart)
          currentTokenStart = i
          currentTokenType = TokenTypes.WHITESPACE
        case '"' =>
          addToken(text, currentTokenStart,i-1, TokenTypes.IDENTIFIER, newStartOffset+currentTokenStart)
          currentTokenStart = i
          currentTokenType = TokenTypes.LITERAL_STRING_DOUBLE_QUOTE
        case _ =>
          // Otherwise, we're still an identifier (?)

      case TokenTypes.LITERAL_NUMBER_DECIMAL_INT =>

        c match 
        case ' ' | '\t' =>
          addToken(text, currentTokenStart,i-1, TokenTypes.LITERAL_NUMBER_DECIMAL_INT, 
                    newStartOffset + currentTokenStart)
          currentTokenStart = i
          currentTokenType = TokenTypes.WHITESPACE
        case '"' => 
          addToken(text, currentTokenStart,i-1, TokenTypes.LITERAL_NUMBER_DECIMAL_INT, 
                    newStartOffset + currentTokenStart)
          currentTokenStart = i
          currentTokenType = TokenTypes.LITERAL_STRING_DOUBLE_QUOTE
        case _ =>
          if RSyntaxUtilities.isDigit(c) then 
            () // Do nothing ; Still a literal number
          else 
            addToken(text, currentTokenStart,i-1, TokenTypes.LITERAL_NUMBER_DECIMAL_INT, 
                      newStartOffset+currentTokenStart);
            i -= 1  // this is uggly
            currentTokenType = TokenTypes.NULL

      case TokenTypes.COMMENT_EOL =>
        i = end - 1
        addToken(text, currentTokenStart,i, currentTokenType, newStartOffset+currentTokenStart)
        // We need to set token type to null so at the bottom we don't add one more token.
        currentTokenType = TokenTypes.NULL

      case TokenTypes.LITERAL_STRING_DOUBLE_QUOTE =>
        if c=='"' then 
          addToken(text, currentTokenStart,i, TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, 
                    newStartOffset+currentTokenStart)
          currentTokenType = TokenTypes.NULL

      case _ => // Should never happen
        //???

      end match

      i += 1
    end while

    currentTokenType match // Remember what token type to begin the next line with
    case TokenTypes.LITERAL_STRING_DOUBLE_QUOTE => 
      addToken(text, currentTokenStart,end-1, currentTokenType, newStartOffset+currentTokenStart)
    case TokenTypes.NULL => // Do nothing if everything was okay.
      addNullToken()
    case _ => // All other token types don't continue to the next line...
      addToken(text, currentTokenStart,end-1, currentTokenType, newStartOffset+currentTokenStart)
      addNullToken()
    end match

    // Return the first token in our linked list.
    firstToken

  end getTokenList
  
end ReqTTokenMaker