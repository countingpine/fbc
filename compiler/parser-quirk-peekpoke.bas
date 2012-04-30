'' quirk pointer statements (PEEK and POKE) parsing
''
'' chng: sep/2004 written [v1ctor]


#include once "fb.bi"
#include once "fbint.bi"
#include once "parser.bi"
#include once "ast.bi"

'':::::
''PokeStmt =   POKE Expression, Expression .
''
function cPokeStmt _
	( _
		_
	) as integer

	dim as ASTNODE ptr expr1 = any, expr2 = any
	dim as integer poketype = any, lgt = any, is_type = any
	dim as FBSYMBOL ptr subtype = any

	function = FALSE

	'' POKE
	lexSkipToken( )

	'' (SymbolType ',')?

	'' next token is TYPEOF?
	'' (We need this to prevent the '(' after it from making it look like an expression)
	if( lexGetToken( ) = FK_TK_TYPEOF ) then
		is_type = cSymbolType( dtype, subtype, lgt )
	
	'' token after next is operator or '('/'['?
	elseif( (lexGetLookAheadClass( 1 ) = FB_TKCLASS_OPERATOR andalso lexGetLookAhead( 1 ) <> CHAR_TIMES) _
		orelse lexGetLookAhead( 1 ) = CHAR_LPRNT ) then
		orelse lexGetLookAhead( 1 ) = CHAR_LBRACKET ) then
		'' disambiguation: types can't be followed by an operator
		'' (note: can't check periods here, because it could be a namespace resolution, or '*' because it could be STRING * n)
		is_type = FALSE
	
	elseif( fbLangIsSet( FB_LANG_QB ) ) then
		'' QB quirk: POKE only takes expressions
			is_type = FALSE
	
	else
		is_type = cSymbolType( dtype, subtype, lgt )
	end if

	if( is_type ) then

		'' check for invalid types
		select case poketype
		case FB_DATATYPE_VOID, FB_DATATYPE_FIXSTR
			errReport( FB_ERRMSG_INVALIDDATATYPES, TRUE )
			'' error recovery: fake a type
			poketype = FB_DATATYPE_UBYTE
			subtype = NULL
		end select

		'' ','
		hMatchCOMMA( )
	else
		poketype = FB_DATATYPE_UBYTE
		subtype  = NULL
	end if

	'' Expression, Expression
	hMatchExpressionEx( expr1, FB_DATATYPE_INTEGER )

	hMatchCOMMA( )

	hMatchExpressionEx( expr2, FB_DATATYPE_INTEGER )

    select case astGetDataClass( expr1 )
    case FB_DATACLASS_STRING
    	errReport( FB_ERRMSG_INVALIDDATATYPES )
    	'' no error recovery: stmt was already parsed
    	astDelTree( expr1 )
        exit function

	case FB_DATACLASS_FPOINT
    	expr1 = astNewCONV( FB_DATATYPE_UINT, NULL, expr1 )

	case else
		if( typeGetSize( astGetDataType( expr1 ) ) <> FB_POINTERSIZE ) then
        	errReport( FB_ERRMSG_INVALIDDATATYPES )
        	'' no error recovery: ditto
        	astDelTree( expr1 )
        	exit function
        end if
	end select

    expr1 = astNewDEREF( expr1, poketype, subtype )

    expr1 = astNewASSIGN( expr1, expr2 )
    if( expr1 = NULL ) then
		errReport( FB_ERRMSG_INVALIDDATATYPES )
	else
		astAdd( expr1 )
	end if

    function = TRUE

end function

'':::::
'' PeekFunct =   PEEK '(' (SymbolType ',')? Expression ')' .
''
function cPeekFunct() as ASTNODE ptr
	dim as ASTNODE ptr expr = any
	dim as integer dtype = any, lgt = any, is_type = any
	dim as FBSYMBOL ptr subtype = any

	function = NULL

	'' PEEK
	lexSkipToken( )

	'' '('
	hMatchLPRNT( )

	'' (SymbolType ',')?

	'' next token is TYPEOF?
	'' (We need this to prevent the '(' after it from making it look like an expression)
	if( lexGetToken( ) = FK_TK_TYPEOF ) then
		is_type = cSymbolType( dtype, subtype, lgt )
	
	'' token after next is operator or '('/'['?
	elseif( (lexGetLookAheadClass( 1 ) = FB_TKCLASS_OPERATOR andalso lexGetLookAhead( 1 ) <> CHAR_TIMES) _
		orelse lexGetLookAhead( 1 ) = CHAR_LPRNT ) then
		orelse lexGetLookAhead( 1 ) = CHAR_LBRACKET ) then
		'' disambiguation: types can't be followed by an operator
		'' (note: can't check periods here, because it could be a namespace resolution, or '*' because it could be STRING * n)
		is_type = FALSE
	
	elseif( fbLangIsSet( FB_LANG_QB ) ) then
		'' QB quirk: PEEK only takes expressions
			is_type = FALSE
	
	else
		is_type = cSymbolType( dtype, subtype, lgt )
	end if

	if( is_type ) then
		'' check for invalid types
		select case typeGet( dtype )
		case FB_DATATYPE_VOID, FB_DATATYPE_FIXSTR
			errReport( FB_ERRMSG_INVALIDDATATYPES )
			'' error recovery: fake a type
			dtype = FB_DATATYPE_UBYTE
			subtype = NULL
		end select

		'' ','
		hMatchCOMMA( )
	else
		dtype = FB_DATATYPE_UBYTE
		subtype = NULL
	end if

	'' Expression
	hMatchExpressionEx( expr, FB_DATATYPE_INTEGER )

	' ')'
	hMatchRPRNT( )

	select case astGetDataClass( expr )
	case FB_DATACLASS_STRING
		errReport( FB_ERRMSG_INVALIDDATATYPES )
		'' error recovery: fake an expr
		astDelTree( expr )
		expr = NULL

	case FB_DATACLASS_FPOINT
		expr = astNewCONV( FB_DATATYPE_UINT, NULL, expr )

	case else
		if( typeGetSize( astGetDataType( expr ) ) <> FB_POINTERSIZE ) then
			errReport( FB_ERRMSG_INVALIDDATATYPES )
			'' error recovery: fake an expr
			astDelTree( expr )
			expr = NULL
		end if
	end select

	if( expr = NULL ) then
		expr = astNewCONSTi( 0, FB_DATATYPE_INTEGER )
	end if

	'' ('.' UdtMember)?
	if( lexGetToken( ) = CHAR_DOT ) then
		select case dtype
		case FB_DATATYPE_STRUCT	', FB_DATATYPE_CLASS

		case else
			errReport( FB_ERRMSG_EXPECTEDUDT, TRUE )
		end select

		lexSkipToken( LEXCHECK_NOPERIOD )
		function = cUdtMember( dtype, subtype, expr, TRUE )
	else
		function = astNewDEREF( expr, dtype, subtype )
	end if
end function
