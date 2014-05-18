package org.fife.ui.rsyntaxtextarea
/*AARGH. Have to puit this stuff in the above package, or else:
src\reqT\parse.scala:67: error: Unable to access method addToken in class TokenMakerBase with a super reference.
      super.addToken(segment, start, end, returnTokenType, startOffset);

It seems to be this know scala compiler bug:  https://issues.scala-lang.org/browse/SI-7936      
      */

  //http://fifesoft.com/rsyntaxtextarea/doc/CustomSyntaxHighlighting.html
  //import org.fife.ui.rsyntaxtextarea.{AbstractTokenMaker, TokenMaker}
  //import org.fife.ui.rsyntaxtextarea.TokenMap
  //import org.fife.ui.rsyntaxtextarea.TokenTypes
  //import org.fife.ui.rsyntaxtextarea.Token
  
class ReqTTokenMaker extends AbstractTokenMaker with TokenMaker {
  override def  getWordsToHighlight(): TokenMap = {
    val tokenMap = new TokenMap()
    
    reqT.metamodel.entityTypes.foreach{ t =>
      tokenMap.put(t.toString, TokenTypes.RESERVED_WORD) }    
      
    reqT.metamodel.attributeTypes.foreach{ t =>
      tokenMap.put(t.toString, TokenTypes.RESERVED_WORD_2) }    
      
    reqT.metamodel.relationTypes.foreach{ t =>
      tokenMap.put(t.toString, TokenTypes.FUNCTION) }

    tokenMap
  }
  
  override def addToken(segment: javax.swing.text.Segment, 
                        start: Int, end: Int, tokenType: Int, startOffset: Int) {
    // This assumes all keywords, etc. were parsed as "identifiers."
    val returnTokenType = tokenType match {
      case TokenTypes.IDENTIFIER => 
        val value = wordsToHighlight.get(segment, start, end)
        if (value != -1) value else tokenType
      case _ => tokenType
    }
    super.addToken(segment, start, end, returnTokenType, startOffset);
  }
  
  override def getTokenList(text: javax.swing.text.Segment, 
                            startTokenType: Int, startOffset: Int): Token = {

                            
    resetTokenList()
    val array = text.array
    val offset = text.offset
    val count = text.count
    val end = offset + count
    // Token starting offsets are always of the form:
    // 'startOffset + (currentTokenStart-offset)', but since startOffset and
    // offset are constant, tokens' starting positions become:
    // 'newStartOffset+currentTokenStart'.
    val newStartOffset = startOffset - offset
    
    var currentTokenStart = offset
    var currentTokenType  = startTokenType
    
    var i = offset
    while (i < end) {
      val c = array(i)
      //STUBB
      addToken(text, currentTokenStart,i-1, TokenTypes.IDENTIFIER, newStartOffset+currentTokenStart)
      currentTokenStart = i
      //END STUBB
      i += 1
    } // end while (i < end) 
                            
    //return
    firstToken
  }
}










