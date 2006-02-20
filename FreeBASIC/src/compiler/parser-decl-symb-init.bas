''	FreeBASIC - 32-bit BASIC Compiler.
''	Copyright (C) 2004-2005 Andre Victor T. Vicentini (av1ctor@yahoo.com.br)
''
''	This program is free software; you can redistribute it and/or modify
''	it under the terms of the GNU General Public License as published by
''	the Free Software Foundation; either version 2 of the License, or
''	(at your option) any later version.
''
''	This program is distributed in the hope that it will be useful,
''	but WITHOUT ANY WARRANTY; without even the implied warranty of
''	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
''	GNU General Public License for more details.
''
''	You should have received a copy of the GNU General Public License
''	along with this program; if not, write to the Free Software
''	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.


'' symbol initializers
''
'' chng: sep/2004 written [v1ctor]

option explicit
option escape

#include once "inc\fb.bi"
#include once "inc\fbint.bi"
#include once "inc\parser.bi"
#include once "inc\ast.bi"
#include once "inc\ir.bi"

declare function 	cSymbUDTInit			( byval basesym as FBSYMBOL ptr, _
											  byval sym as FBSYMBOL ptr, _
											  byref ofs as integer, _
					   						  byval isstatic as integer ) as integer


'':::::
function cSymbElmInit( byval basesym as FBSYMBOL ptr, _
					   byval sym as FBSYMBOL ptr, _
					   byref ofs as integer, _
					   byval isstatic as integer ) as integer

	dim as integer dtype, sdtype
	dim as ASTNODE ptr expr, assgexpr
    dim as FBSYMBOL ptr litsym, oldsym

    sdtype = symbGetType( sym )

    '' set the context symbol to allow taking the address of overloaded
    '' procs and also to allow anonymous UDT's
    oldsym = env.ctxsym
    env.ctxsym = symbGetSubType( sym )

	if( cExpression( expr ) = FALSE ) then
		hReportError( FB_ERRMSG_EXPECTEDEXPRESSION )
		return 0
	end if

	env.ctxsym = oldsym

    dtype = astGetDataType( expr )

	'' if static, only constants can be used as initializers
	if( isstatic ) then

		'' check if it's a literal string
		litsym = NULL
		select case dtype
		case FB_DATATYPE_CHAR, FB_DATATYPE_WCHAR
			litsym = astGetStrLitSymbol( expr )
		end select

		'' not a literal string?
		if( litsym = NULL ) then

			'' string?
			if( hIsString( sdtype ) ) then
				if( hIsString( dtype ) ) then
					hReportError( FB_ERRMSG_EXPECTEDCONST, TRUE )
				else
					hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
				end if
				return 0

			elseif( hIsString( dtype ) ) then
			    hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
				return 0
			end if

			'' bit field?
			if( symbIsUDTElm( sym ) ) then
			    if( symbGetType( sym ) = FB_DATATYPE_BITFIELD ) then
			    	hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
					return 0
				end if
			end if

			'' offset?
			if( astIsOFFSET( expr ) ) then

				'' different types?
				if( (symbGetDataClass( sdtype ) <> FB_DATACLASS_INTEGER) or _
					(symbGetDataSize( sdtype ) <> FB_POINTERSIZE) ) then
					hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
					return 0
				end if

				irEmitVARINIOFS( astGetSymbol( expr ) )

			else
				'' not a constant?
				if( astIsCONST( expr ) = FALSE ) then
					hReportError( FB_ERRMSG_EXPECTEDCONST, TRUE )
					return 0
				end if

				'' different types?
				if( dtype <> sdtype ) then
					expr = astNewCONV( INVALID, _
									   sdtype, _
									   symbGetSubtype( sym ), _
									   expr )

					if( expr = NULL ) then
			    		hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
						return 0
					end if

				end if

				select case as const sdtype
				case FB_DATATYPE_LONGINT, FB_DATATYPE_ULONGINT
					irEmitVARINI64( sdtype, astGetValLong( expr ) )
				case FB_DATATYPE_SINGLE, FB_DATATYPE_DOUBLE
					irEmitVARINIf( sdtype, astGetValFloat( expr ) )
				case else
					irEmitVARINIi( sdtype, astGetValInt( expr ) )
				end select

			end if

		else

			'' not a string?
			if( hIsString( sdtype ) = FALSE ) then
				hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
				return 0
			end if

			'' can't be a variable-len string
			if( sdtype = FB_DATATYPE_STRING ) then
				hReportError( FB_ERRMSG_CANTINITDYNAMICSTRINGS, TRUE )
				return 0
			end if

			'' not a wstring?
			if( sdtype <> FB_DATATYPE_WCHAR ) then

				'' convert?
				if( dtype <> FB_DATATYPE_WCHAR ) then
					'' less the null-char
					irEmitVARINISTR( symbGetStrLen( sym ) - 1, _
							 	 	 symbGetVarText( litsym ), _
							 	 	 symbGetStrLen( litsym ) - 1 )
				else
					'' ditto
					irEmitVARINISTR( symbGetStrLen( sym ) - 1, _
							 	 	 str( *symbGetVarTextW( litsym ) ), _
							 	 	 symbGetWstrLen( litsym ) - 1 )
				end if


			'' wstring..
			else

				'' convert?
				if( dtype <> FB_DATATYPE_WCHAR ) then
					'' less the null-char
					irEmitVARINIWSTR( symbGetWstrLen( sym ) - 1, _
							 	  	  wstr( *symbGetVarText( litsym ) ), _
							 	  	  symbGetStrLen( litsym ) - 1 )
				else
					'' ditto
					irEmitVARINIWSTR( symbGetWstrLen( sym ) - 1, _
							 	  	  symbGetVarTextW( litsym ), _
							 	  	  symbGetWstrLen( litsym ) - 1 )
				end if

			end if

		end if

		astDel( expr )

	else

        assgexpr = astNewVAR( basesym, ofs, sdtype, symbGetSubtype( sym ) )

        '' field?
        if( symbIsUDTElm( sym ) ) then
        	assgexpr = astNewFIELD( assgexpr, sym, sdtype, symbGetSubtype( sym ) )
        end if

        assgexpr = astNewASSIGN( assgexpr, expr )

        if( assgexpr = NULL ) then
			hReportError( FB_ERRMSG_INVALIDDATATYPES, TRUE )
            return FALSE
        end if

        astAdd( assgexpr )

	end if

	ofs += symbGetLen( sym )

	function = symbGetLen( sym )

end function

'':::::
function cSymbArrayInit( byval basesym as FBSYMBOL ptr, _
						 byval sym as FBSYMBOL ptr, _
					     byref ofs as integer, _
					     byval isstatic as integer, _
					     byval isarray as integer _
					   ) as integer

    dim as integer dimensions, dimcnt, elements, elmcnt
    dim as integer isopen, lgt, pad
    dim as FBVARDIM ptr d, ld

	function = 0

	dimensions = symbGetArrayDimensions( sym )
    dimcnt = 0

	if( isarray ) then
		d = symbGetArrayFirstDim( sym )
	else
		d = NULL
	end if

	lgt = 0

	'' for each array dimension..
	do
		'' '{'?
		isopen = FALSE
		if( isarray ) then
			if( hMatch( CHAR_LBRACE ) ) then
				dimcnt += 1
				if( dimcnt > dimensions ) then
					hReportError( FB_ERRMSG_TOOMANYEXPRESSIONS )
					exit function
				end if

				ld = d
				d = d->next

				isopen = TRUE
			end if
		end if

		if( d <> NULL ) then
			elements = (d->upper - d->lower) + 1
		else
			elements = 1
		end if

		'' for each array element..
		elmcnt = 0
		do

			if( symbGetType( sym ) <> FB_DATATYPE_USERDEF ) then
				if( cSymbElmInit( basesym, sym, ofs, isstatic ) = 0 ) then
					exit function
				end if
			else
				if( cSymbUDTInit( basesym, sym, ofs, isstatic ) = 0 ) then
					exit function
				end if
			end if

			elmcnt += 1
			if( elmcnt >= elements ) then
				exit do
			end if

		'' ','
		loop while( hMatch( CHAR_COMMA ) )

		'' pad
		if( isstatic ) then
			if( elmcnt < elements ) then
				irEmitVARINIPAD( (elements - elmcnt) * symbGetLen( sym ) )
			end if
			lgt += elements * symbGetLen( sym )
		else
			lgt += elmcnt * symbGetLen( sym )
		end if

		if( isopen = FALSE ) then
			exit do
		end if

		if( isarray ) then
			'' '}'?
			if( hMatch( CHAR_RBRACE ) = FALSE ) then
				hReportError( FB_ERRMSG_EXPECTEDRBRACKET )
				exit function
			end if

			dimcnt -= 1
			d = ld
		end if

	'' ','
	loop while( hMatch( CHAR_COMMA ) )

	'' pad
	pad = (symbGetLen( sym ) * symbCalcArrayElements( sym )) - lgt
	if( pad > 0 ) then
		if( isstatic ) then
			irEmitVARINIPAD( pad )
		else
			ofs += pad
		end if
	end if

	function = lgt + pad

end function

'':::::
function cSymbUDTInit( byval basesym as FBSYMBOL ptr, _
					   byval sym as FBSYMBOL ptr, _
					   byref ofs as integer, _
					   byval isstatic as integer ) as integer

	dim as integer elements, elmcnt, isarray, elmofs, lgt, pad
    dim as FBSYMBOL ptr elm, udt

    function = 0

	'' '('
	if( hMatch( CHAR_LPRNT ) = FALSE ) then
		'' it can be a function returning an UDT or another UDT
		'' variable for non-static symbols..
		return cSymbElmInit( basesym, sym, ofs, isstatic )
	end if

	udt = symbGetSubtype( sym )
	elm = symbGetUDTFirstElm( udt )

	elements = symbGetUDTElements( udt )
	elmcnt = 0

	lgt = 0

	'' for each UDT element..
	do
		elmcnt += 1
		if( elmcnt > elements ) then
			hReportError( FB_ERRMSG_TOOMANYEXPRESSIONS )
			exit function
		end if

		'' '{'?
		isarray = hMatch( CHAR_LBRACE )

		elmofs = elm->var.elm.ofs
		if( isstatic ) then
			if( lgt > 0 ) then
				pad = elmofs - lgt
				if( pad > 0 ) then
					irEmitVARINIPAD( pad )
					lgt += pad
				end if
			end if
		end if

		elmofs += ofs

        pad = cSymbArrayInit( basesym, elm, elmofs, isstatic, isarray )
        if( pad = 0 ) then
          	exit function
        end if

        lgt += pad

        if( isarray ) then
			'' '}'
			if( hMatch( CHAR_RBRACE ) = FALSE ) then
				hReportError( FB_ERRMSG_EXPECTEDRBRACKET )
				exit function
			end if
		end if

		'' next
		elm = symbGetUDTNextElm( elm )

	'' ','
	loop while( hMatch( CHAR_COMMA ) )

	ofs += symbGetLen( sym )

	'' ')'
	if( hMatch( CHAR_RPRNT ) = FALSE ) then
		hReportError( FB_ERRMSG_EXPECTEDRPRNT )
		exit function
	end if

	'' pad
	if( isstatic ) then
		pad = symbGetLen( sym ) - lgt
		if( pad > 0 ) then
			irEmitVARINIPAD( pad )
		end if
	else
		pad = 0
	end if

	function = lgt + pad

end function

'':::::
function cSymbolInit( byval sym as FBSYMBOL ptr ) as integer
    dim as integer isarray, ofs

	function = FALSE

	'' cannot initialize dynamic vars
	if( symbGetIsDynamic( sym ) ) then
		hReportError( FB_ERRMSG_CANTINITDYNAMICARRAYS, TRUE )
		exit function
	end if

	'' common?? impossible but..
	if( symbIsCommon( sym ) ) then
		hReportError( FB_ERRMSG_CANTINITDYNAMICARRAYS, TRUE )
		exit function
	end if

	'' already emited?? impossible but..
	if( symbGetIsEmitted( sym ) ) then
		hReportError( FB_ERRMSG_SYNTAXERROR, TRUE )
		exit function
	end if

	symbSetIsEmitted( sym )

	''
	if( symbIsStatic( sym ) ) then
		irEmitVARINIBEGIN( sym )
	end if

	'' '{'?
	isarray = hMatch( CHAR_LBRACE )

	ofs = 0

	if( cSymbArrayInit( sym, sym, ofs, symbIsStatic( sym ), isarray ) = 0 ) then
		exit function
	end if

    if( isarray ) then
		'' '}'
		if( hMatch( CHAR_RBRACE ) = FALSE ) then
			hReportError( FB_ERRMSG_EXPECTEDRBRACKET )
			exit function
		end if
	end if

	''
	if( symbIsStatic( sym ) ) then
		irEmitVARINIEND( sym )
	end if

	''
	function = TRUE

end function

