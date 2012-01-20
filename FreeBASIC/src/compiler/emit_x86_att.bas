''	FreeBASIC - 32-bit BASIC Compiler.
''	Copyright (C) 2004-2010 The FreeBASIC development team.
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


'' code generation for x86, GNU assembler (GAS/Intel arch)
''
'' chng: sep/2004 written [v1ctor]
''  	 mar/2005 longint support added [v1ctor]
''  	 may/2008 SSE/SSE2 instructions [Bryan Stoeberl]


#include once "inc\fb.bi"
#include once "inc\fbint.bi"
#include once "inc\reg.bi"
#include once "inc\ir.bi"
#include once "inc\rtl.bi"
#include once "inc\emit.bi"
#include once "inc\emitdbg.bi"
#include once "inc\hash.bi"
#include once "inc\symb.bi"

const EMIT_MEMBLOCK_MAXLEN	= 16				'' when to use memblk clear/move (needed by AST)

''
type EMITDATATYPE
	class			as integer
	size			as integer
	rnametb			as integer
	mname			as zstring * 11+1
end type

type EMITDATATYPEEX
	class			as integer
	size			as integer
	rnametb			as integer
	mname			as zstring * 11+1
	mfl				as zstring * 1+1
end type

const EMIT_MAXRNAMES  = REG_MAXREGS
const EMIT_MAXRTABLES = 4				'' 8-bit, 16-bit, 32-bit, fpoint

''
const EMIT_LOCSTART 	= 0
const EMIT_ARGSTART 	= FB_POINTERSIZE + FB_INTEGERSIZE '' skip return address + saved ebp

''
enum EMITREG_ENUM
	EMIT_REG_FP0	= 0
	EMIT_REG_FP1
	EMIT_REG_FP2
	EMIT_REG_FP3
	EMIT_REG_FP4
	EMIT_REG_FP5
	EMIT_REG_FP6
	EMIT_REG_FP7

	EMIT_REG_EDX	= EMIT_REG_FP0				'' aliased
	EMIT_REG_EDI
	EMIT_REG_ESI
	EMIT_REG_ECX
	EMIT_REG_EBX
	EMIT_REG_EAX
	EMIT_REG_EBP
	EMIT_REG_ESP
end enum


const COMMA   = ", "


declare sub hDeclVariable _
	( _
		byval s as FBSYMBOL ptr _
	)

'' from emit_SSE.bas
declare function _init_opFnTB_SSE _
	( _
		byval _opFnTB_SSE as any ptr ptr _
	) as integer

'' from emit_x86.bas
declare function emitGasX86_ctor _
	( _
	) as integer

declare function hIsRegFree _
	( _
		byval dclass as integer, _
		byval reg as integer _
	) as integer

declare function hFindRegNotInVreg _
	( _
		byval vreg as IRVREG ptr, _
		byval noSIDI as integer = FALSE _
	) as integer

declare function hFindFreeReg _
	( _
		byval dclass as integer _
	) as integer

declare function hIsRegInVreg _
	( _
		byval vreg as IRVREG ptr, _
		byval reg as integer _
	) as integer

declare sub outp _
	( _
		byval s as zstring ptr _
	)

declare sub hBRANCH _
	( _
		byval mnemonic as zstring ptr, _
		byval label as zstring ptr _
	)

declare sub hLABEL _
	( _
		byval label as zstring ptr _
	)

''globals

	'' same order as EMITREG_ENUM
	dim shared rnameattTB(0 to EMIT_MAXRTABLES-1, 0 to EMIT_MAXRNAMES-1) as zstring * 8+1 => _
	{ _
		{    "%dl",   "%di",   "%si",   "%cl",   "%bl",   "%al",   "%bp",   "%sp" }, _
		{    "%dx",   "%di",   "%si",   "%cx",   "%bx",   "%ax",   "%bp",   "%sp" }, _
		{   "%edx",  "%edi",  "%esi",  "%ecx",  "%ebx",  "%eax",  "%ebp",  "%esp" }, _
		{ "%st(0)","%st(1)","%st(2)","%st(3)","%st(4)","%st(5)","%st(6)","%st(7)" } _
	}

	'' same order as FB_DATATYPE
	extern dtsuffixTB(0 to FB_DATATYPES-1) as EMITDATATYPEEX
	extern dtypeTB(0 to FB_DATATYPES-1) as EMITDATATYPE

	dim shared dtsuffixTB(0 to FB_DATATYPES-1) as EMITDATATYPEEX => _
	{ _
		( FB_DATACLASS_INTEGER, 0 			    , 0, "" , ""  ), _	'' void
		( FB_DATACLASS_INTEGER, 1			    , 0, "b", ""  ), _	'' byte
		( FB_DATACLASS_INTEGER, 1			    , 0, "b", ""  ), _	'' ubyte
		( FB_DATACLASS_INTEGER, 1               , 0, "b", ""  ), _	'' char
		( FB_DATACLASS_INTEGER, 2               , 1, "w", ""  ), _	'' short
		( FB_DATACLASS_INTEGER, 2               , 1, "w", ""  ), _	'' ushort
		( FB_DATACLASS_INTEGER, 2  				, 1, "w", ""  ), _	'' wchar
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE  , 2, "l", ""  ), _	'' int
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE  , 2, "l", ""  ), _   '' uint
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE  , 2, "l", ""  ), _	'' enum
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE  , 2, "l", ""  ), _	'' bitfield
		( FB_DATACLASS_INTEGER, FB_LONGSIZE  	, 2, "l", ""  ), _	'' long
		( FB_DATACLASS_INTEGER, FB_LONGSIZE  	, 2, "l", ""  ), _   '' ulong
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE*2, 2, "q", ""  ), _	'' longint
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE*2, 2, "q", ""  ), _	'' ulongint
		( FB_DATACLASS_FPOINT , 4			    , 3, "l", "s" ), _	'' single
		( FB_DATACLASS_FPOINT , 8			    , 3, "q", "l" ), _	'' double
		( FB_DATACLASS_STRING , FB_STRDESCLEN	, 0, "" , ""  ), _	'' string
		( FB_DATACLASS_STRING , 1               , 0, "b", ""  ), _	'' fix-len string
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE  , 2, "l", ""  ), _	'' struct
		( FB_DATACLASS_INTEGER, 0  				, 0, "" , ""  ), _	'' namespace
		( FB_DATACLASS_INTEGER, FB_INTEGERSIZE  , 2, "l", ""  ), _	'' function
		( FB_DATACLASS_INTEGER, 1			    , 0, "b", ""  ), _	'' fwd-ref
		( FB_DATACLASS_INTEGER, FB_POINTERSIZE  , 2, "l", ""  ), _	'' pointer
		( FB_DATACLASS_INTEGER, 16              , 3, "" , ""  ) _		'' 128-bit 
	}

''const EMIT_MAXKEYWORDS = 600

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' helper functions
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
#define hEmitBssHeader( ) emitSection( IR_SECTION_BSS, 0 )

'':::::
#ifdef __FB_LINUX__
''
'' !!!FIXME!!!
''
'' Linux appears to support .rodata section, but I'm not sure about other platforms, and that's
'' probably the reason FB used to output a normal .data section in any case...
''
#define hEmitConstHeader( ) emitSection( IR_SECTION_CONST, 0 )
#else
#define hEmitConstHeader( ) emitSection( IR_SECTION_DATA, 0 )
#endif

'':::::
#define hEmitDataHeader( ) emitSection( IR_SECTION_DATA, 0 )

'':::::
#define hEmitExportHeader( ) emitSection( IR_SECTION_DIRECTIVE, 0 )

'':::::
function hGetRegNameAtt _
	( _
		byval dtype as integer, _
		byval reg as integer _
	) as zstring ptr

	if( reg = INVALID ) then
		function = NULL
	else
		dim as integer tb = dtypeTB(typeGet( dtype )).rnametb

		function = @rnameattTB(tb, reg)
	end if

end function

'':::::
private function hGetIdxName _
	( _
		byval vreg as IRVREG ptr, _
		byval ofs as integer _
	) as zstring ptr static
	
	static as zstring * FB_MAXINTNAMELEN+1+8+1+1+1+1+8+1 iname
	dim as FBSYMBOL ptr sym
	dim as IRVREG ptr vi
	dim as integer addone, mult
	dim as zstring ptr rname
	
	sym = vreg->sym
	vi = vreg->vidx
	
	mult = vreg->mult
	if( mult > 1 ) then
		addone = FALSE
		select case mult
		case 3, 5, 9
			mult -= 1
			addone = TRUE
		end select
	end if
	
	if( vi <> NULL ) then
		rname = hGetRegNameAtt( vi->dtype, vi->reg )
	end if
	
	if( sym = NULL ) then
		'' no var or index?
		if( vi = NULL ) then
			return NULL
		end if
		
		iname = "("
		
	else
		if( ofs <> 0 ) then
			iname = str( ofs )
		else
			iname = ""
		end if
		iname += "("
		dim as zstring ptr tmpname
		tmpname = symbGetMangledName( sym )
		if( tmpname[0] <> asc("%") ) then
			iname = *tmpname + iname
			if( addone ) then
				iname += *rname
			end if
		else
			iname += *tmpname
		end if
		if( vi <> NULL ) then
			iname += ", "
		end if
	end if

	iname += *rname

    if( vi <> NULL ) then
		if( mult > 1 ) then
			iname += ", "
			iname += str( mult )
		end if
	end if
	
	iname += ")"

	function = @iname

end function

'' disp(base,offset,scalar) = [base+disp+offset*scalar]
'':::::
sub hPrepOperandAtt _
	( _
		byval vreg as IRVREG ptr, _
		byref operand as string, _
		byval dtype as FB_DATATYPE = FB_DATATYPE_INVALID, _
		byval ofs as integer = 0, _
		byval isaux as integer = FALSE, _
		byval addprefix as integer = TRUE _
	) static

    if( vreg = NULL ) then
    	operand = ""
    	exit sub
    end if

    if( dtype = FB_DATATYPE_INVALID ) then
    	dtype = vreg->dtype
    end if

	select case as const vreg->typ
	case IR_VREGTYPE_VAR, IR_VREGTYPE_PTR
	
		operand = ""
		
		'' offset
		ofs += vreg->ofs
		if( isaux ) then
			ofs += FB_INTEGERSIZE
		end if
		
		'' variable or index
		dim as zstring ptr idx_op
		if( vreg->typ = IR_VREGTYPE_VAR ) then
			idx_op = symbGetMangledName( vreg->sym )
		else
        	idx_op = hGetIdxName( vreg, ofs )
		end if
		
        if( idx_op <> NULL ) then
			if( vreg->typ = IR_VREGTYPE_VAR ) then
				if( idx_op[0] = asc("%") ) then
					if( ofs <> 0 ) then
						operand = str( ofs )
					end if
					
					operand += "(" + *idx_op + ")"
				else
					operand = *idx_op
					if( ofs > 0 ) then
						operand += " + "
					end if
					if( ofs <> 0 ) then
						operand += str( ofs )
					end if
				end if
			else
				operand = *idx_op
			end if
		else
			operand = "(, " + str( ofs ) + ")"
			''TODO: not sure what this line is supposed to do
        end if
	
	case IR_VREGTYPE_IDX
		
		operand = ""
		
		'' offset
		ofs += vreg->ofs
		if( isaux ) then
			ofs += FB_INTEGERSIZE
		end if
		
		dim as zstring ptr idx_op
		idx_op = hGetIdxName( vreg, ofs )
		
		if( idx_op <> NULL ) then
			operand = *idx_op
		end if

	case IR_VREGTYPE_OFS
		operand = "$"
		operand += *symbGetMangledName( vreg->sym )
		if( vreg->ofs <> 0 ) then
			operand += " + "
			operand += str( vreg->ofs )
		end if

	case IR_VREGTYPE_REG
		if( isaux = FALSE ) then
			operand = *hGetRegNameAtt( dtype, vreg->reg )
		else
			operand = *hGetRegNameAtt( dtype, vreg->vaux->reg )
		end if

	case IR_VREGTYPE_IMM
		if( isaux = FALSE ) then
			operand = "$" + str( vreg->value.int )
		else
			operand = "$" + str( vreg->vaux->value.int )
		end if

	case else
    	operand = ""
	end select

end sub

'':::::
sub hPrepOperandAtt64 _
	( _
		byval vreg as IRVREG ptr, _
		byref operand1 as string, _
		byref operand2 as string _
	) static

	hPrepOperandAtt( vreg, operand1, FB_DATATYPE_UINT   , 0, FALSE )
	hPrepOperandAtt( vreg, operand2, FB_DATATYPE_INTEGER, 0, TRUE )

end sub

'':::::
private sub outEx _
	( _
		byval s as zstring ptr, _
		byval bytes as integer = 0 _
	) static

	if( bytes = 0 ) then
		bytes = len( *s )
	end if

	if( put( #env.outf.num, , *s, bytes ) = 0 ) then
	end if

end sub

'':::::
sub hPUSHAtt _
	( _
		byval rname as zstring ptr _
	) static

    dim ostr as string

	ostr = "pushl "
	ostr += *rname
	outp( ostr )

end sub

'':::::
sub hPOPAtt _
	( _
		byval rname as zstring ptr _
	) static

    dim ostr as string

    ostr = "pop "
    ostr += *rname
	outp( ostr )

end sub

'':::::
'' Careful to pass the datatype for non-dword values
sub hMOVAtt _
	( _
		byval dname as zstring ptr, _
		byval sname as zstring ptr, _
		byval dtype as integer = FB_DATATYPE_INTEGER _
	) static

    dim ostr as string

	ostr = "mov"
	ostr += dtsuffixTB(dtype).mname + " "
	ostr += *sname
	ostr += ", "
	ostr += *dname
	outp( ostr )

end sub

'':::::
'' Note passing the datatype for the suffix like hMOVAtt
private sub hXCHGAtt _
	( _
		byval dname as zstring ptr, _
		byval sname as zstring ptr, _
		byval dtype as integer = FB_DATATYPE_INTEGER _
	) static

    dim ostr as string

	ostr = "xchg"
	ostr += dtsuffixTB(dtype).mname + " "
	ostr += *sname
	ostr += ", "
	ostr += *dname
	outp( ostr )

end sub

'':::::
private sub hCOMMENT _
	( _
		byval s as zstring ptr _
	) static

    dim ostr as string

    ostr = TABCHAR + "#"
    ostr += *s
    ostr += NEWLINE
	outEX( ostr )

end sub

'':::::
private sub hPUBLIC _
	( _
		byval label as zstring ptr, _
		byval isexport as integer _
	) static

    dim ostr as string

	ostr = NEWLINE + ".globl "
	ostr += *label

' PENDING: shared lib compatibility between win32/linux
'          rtlib/gfxlib needs -fvisibility=hidden, only available in gcc 4
'	if( env.clopt.target = FB_COMPTARGET_LINUX ) then
'		if( isexport ) then
'			ostr += NEWLINE + ".protected "
'			ostr += *label
'		else
'			ostr += NEWLINE + ".hidden "
'			ostr += *label
'		end if
'	end if

	ostr += NEWLINE
	outEx( ostr )

end sub

'':::::
private sub hALIGN _
	( _
		byval bytes as integer _
	) static

    dim ostr as string

    ostr = ".balign " + str( bytes ) + NEWLINE
	outEx( ostr )

end sub

'':::::
private sub hInitRegTB
	dim as integer lastclass, regs, i, j

	'' ebp and esp are reserved
	const int_regs = 6

	static as REG_SIZEMASK int_bitsmask(0 to int_regs-1) = _
	{ _
		REG_SIZEMASK_8 or REG_SIZEMASK_16 or REG_SIZEMASK_32, _		'' edx
						  REG_SIZEMASK_16 or REG_SIZEMASK_32, _		'' edi
						  REG_SIZEMASK_16 or REG_SIZEMASK_32, _		'' esi
		REG_SIZEMASK_8 or REG_SIZEMASK_16 or REG_SIZEMASK_32, _		'' ecx
		REG_SIZEMASK_8 or REG_SIZEMASK_16 or REG_SIZEMASK_32, _		'' ebx
		REG_SIZEMASK_8 or REG_SIZEMASK_16 or REG_SIZEMASK_32 _		'' eax
	}

	emit.regTB(FB_DATACLASS_INTEGER) = _
		regNewClass( FB_DATACLASS_INTEGER, _
					 int_regs, _
					 int_bitsmask( ), _
					 FALSE )

	'' no st(7) as STORE/LOAD/POW/.. need a free reg to work
	const flt_regs = 7

	static as REG_SIZEMASK flt_bitsmask(0 to flt_regs-1) = _
	{ _
		REG_SIZEMASK_32 or REG_SIZEMASK_64, _						'' st(0)
		REG_SIZEMASK_32 or REG_SIZEMASK_64, _						'' st(1)
		REG_SIZEMASK_32 or REG_SIZEMASK_64, _						'' st(2)
		REG_SIZEMASK_32 or REG_SIZEMASK_64, _						'' st(3)
		REG_SIZEMASK_32 or REG_SIZEMASK_64, _						'' st(4)
		REG_SIZEMASK_32 or REG_SIZEMASK_64, _						'' st(5)
		REG_SIZEMASK_32 or REG_SIZEMASK_64 _						'' st(6)
	}

	'' create non-stacked floating-point registers
	if( env.clopt.fputype = FB_FPUTYPE_SSE ) then
		emit.regTB(FB_DATACLASS_FPOINT) = _
			regNewClass( FB_DATACLASS_FPOINT, _
						 flt_regs, _
						 flt_bitsmask( ), _
						 FALSE )

		'' change floating-point register names to SSE registers
		for i = 0 to EMIT_MAXRNAMES - 1
			rnameattTB(EMIT_MAXRTABLES-1, i) = "%xmm" + Str(i)
		next i
	else
		emit.regTB(FB_DATACLASS_FPOINT) = _
			regNewClass( FB_DATACLASS_FPOINT, _
						 flt_regs, _
						 flt_bitsmask( ), _
						 TRUE )
	end if
	
end sub

'':::::
private sub hEndRegTB
    dim i as integer

	for i = 0 to EMIT_REGCLASSES-1
		regDelClass( emit.regTB(i) )
	next

end sub

'':::::
private sub hEmitVarBss _
	( _
		byval s as FBSYMBOL ptr _
	) static

    dim as string alloc, ostr
    dim as integer attrib, elements

	attrib = symbGetAttrib( s )

	elements = 1
    if( symbGetArrayDimensions( s ) > 0 ) then
    	elements = symbGetArrayElements( s )
	end if

    hEmitBssHeader( )

    '' allocation modifier
    if( (attrib and FB_SYMBATTRIB_COMMON) = 0 ) then
      	if( (attrib and FB_SYMBATTRIB_PUBLIC) > 0 ) then
       		hPUBLIC( *symbGetMangledName( s ), TRUE )
		end if
       	alloc = ".lcomm"
	else
       	hPUBLIC( *symbGetMangledName( s ), FALSE )
       	alloc = ".comm"
    end if

    '' align
    if( symbGetType( s ) = FB_DATATYPE_DOUBLE ) then
    	hALIGN( 8 )
    	emitWriteStr( ".balign 8", TRUE )
	else
    	hALIGN( 4 )
    end if

	'' emit
    ostr = alloc + TABCHAR
    ostr += *symbGetMangledName( s )
    ostr += "," + str( symbGetLen( s ) * elements )
    emitWriteStr( ostr, TRUE )

    '' add dbg info, if public or shared
    if( (attrib and (FB_SYMBATTRIB_SHARED or _
    				 FB_SYMBATTRIB_COMMON or _
    				 FB_SYMBATTRIB_PUBLIC)) > 0 ) then
    	edbgEmitGlobalVar( s, IR_SECTION_BSS )
	end if

end sub

'':::::
private sub hWriteHeader( ) static

	''
	edbgEmitHeader( env.inf.name )

	''
    hCOMMENT( env.inf.name + "' compilation started at " + time + " (" + FB_SIGN + ")" )

end sub

'':::::
private sub hWriteFooter _
	( _
		byval tottime as double _
	) static

	hCOMMENT( env.inf.name + "' compilation took " + str( tottime ) + " secs" )

	''
	edbgIncludeEnd( )

end sub

'':::::
private sub hWriteBss _
	( _
		byval s as FBSYMBOL ptr )

    do while( s <> NULL )

    	select case symbGetClass( s )
		'' name space?
		case FB_SYMBCLASS_NAMESPACE
			hWriteBss( symbGetNamespaceTbHead( s ) )

		'' scope block?
		case FB_SYMBCLASS_SCOPE
			hWriteBss( symbGetScopeSymbTbHead( s ) )

    	'' variable?
    	case FB_SYMBCLASS_VAR
    		hDeclVariable( s )

    	end select

    	s = s->next
    loop

end sub

'':::::
private sub hEmitVarConst _
	( _
		byval s as FBSYMBOL ptr _
	) static

	dim as string stext, stype, ostr
	dim as integer dtype

	dtype = symbGetType( s )

	select case as const dtype
	case FB_DATATYPE_CHAR
		stext = QUOTE
		stext += *hEscape( symbGetVarLitText( s ) )
		stext += RSLASH + "0" + QUOTE

	case FB_DATATYPE_WCHAR
		stext = QUOTE
		stext += *hEscapeW( symbGetVarLitTextW( s ) )
		stext += *hGetWstrNull( )
		stext += QUOTE

	case else
		stext = "$"
		stext += *symbGetVarLitText( s )
	end select

	hEmitConstHeader( )


	'' some SSE instructions require operands to be 16-byte aligned
	if( s->var_.align ) then
		hALIGN ( s->var_.align )
	else
		if( dtype = FB_DATATYPE_DOUBLE ) then
			hALIGN( 8 )
			else
			hALIGN( 4 )
		end if
	end if


	stype = *emit.vtbl.getTypeString( dtype )
	ostr = *symbGetMangledName( s )
	ostr += (":" + TABCHAR) + stype + TABCHAR + stext
	emitWriteStr( ostr )

end sub

'':::::
private sub hWriteConst _
	( _
		byval s as FBSYMBOL ptr )

	do while( s <> NULL )

		select case symbGetClass( s )
		'' name space?
		case FB_SYMBCLASS_NAMESPACE
			hWriteConst( symbGetNamespaceTbHead( s ) )

		'' scope block?
		case FB_SYMBCLASS_SCOPE
			hWriteConst( symbGetScopeSymbTbHead( s ) )

		'' variable?
		case FB_SYMBCLASS_VAR
			hDeclVariable( s )
		end select

		s = s->next
	loop

end sub

'':::::
private sub hWriteData _
	( _
		byval s as FBSYMBOL ptr )

	do while( s <> NULL )

		select case symbGetClass( s )
		'' name space?
		case FB_SYMBCLASS_NAMESPACE
			hWriteData( symbGetNamespaceTbHead( s ) )

		'' scope block?
		case FB_SYMBCLASS_SCOPE
			hWriteData( symbGetScopeSymbTbHead( s ) )

		'' variable?
		case FB_SYMBCLASS_VAR
			hDeclVariable( s )

		end select

		s = s->next
	loop

end sub

'':::::
private sub hWriteCtor _
	( _
		byval proc_head as FB_GLOBCTORLIST_ITEM ptr, _
		byval is_ctor as integer _
	)

    if( proc_head = NULL ) then
    	exit sub
    end if

    do
    	'' was it emitted?
    	if( symbGetProcIsEmitted( proc_head->sym ) ) then
    		emitSection( iif( is_ctor, _
    						  IR_SECTION_CONSTRUCTOR, _
    						  IR_SECTION_DESTRUCTOR ), _
    					 symbGetProcPriority( proc_head->sym ) )
    		emitVARINIOFS( symbGetMangledName( proc_head->sym ), 0 )
    	end if

    	proc_head = proc_head->next
    loop while( proc_head <> NULL )

end sub

private sub hEmitExport( byval s as FBSYMBOL ptr )
    if( symbIsExport( s ) ) then
        hEmitExportHeader( )

        dim as zstring ptr sname = symbGetMangledName( s )
        if( env.target.underprefix ) then
            sname += 1
        end if

        emitWriteStr( ".ascii " + QUOTE + " -export:" + _
                      *sname + (QUOTE + NEWLINE), _
                      TRUE )
    end if
end sub

private sub hWriteExport( byval s as FBSYMBOL ptr )

    '' for each proc exported..
    do while( s )

    	select case symbGetClass( s )
		'' name space?
		case FB_SYMBCLASS_NAMESPACE
			hWriteExport( symbGetNamespaceTbHead( s ) )

		case FB_SYMBCLASS_STRUCT
			'' does struct have exports?
			dim as FBSYMBOL ptr walk = symbGetUDTSymbTbHead( s )
			while( walk )
                hEmitExport( walk )
				walk = symbGetNext( walk )
			wend

    	case FB_SYMBCLASS_PROC
    		if( symbGetIsDeclared( s ) ) then
                hEmitExport( s )
    		end if
    	end select

    	s = s->next
    loop

end sub

'':::::
private sub hDeclVariable _
	( _
		byval s as FBSYMBOL ptr _
	) static

    '' already allocated?
	if( symbGetVarIsAllocated( s ) ) then
		return
	end if

	symbSetVarIsAllocated( s )

	'' literal?
    if( symbGetIsLiteral( s ) ) then

    	select case symbGetType( s )
    	'' udt? don't emit
    	case FB_DATATYPE_STRUCT
    		return

    	'' string? check if ever referenced
    	case FB_DATATYPE_CHAR, FB_DATATYPE_WCHAR
    	  	if( symbGetIsAccessed( s ) = FALSE ) then
    	  		return
    	  	end if

		'' anything else, only if len > 0
		case else
			if( symbGetLen( s ) <= 0 ) then
				return
			end if
    	end select

    	hEmitVarConst( s )

    	return
	end if

	'' initialized?
	if( symbGetIsInitialized( s ) ) then

		'' extern or jump-tb?
    	if( symbIsExtern( s ) ) then
			return
		elseif( symbGetIsJumpTb( s ) ) then
			return
		end if

    	'' never referenced?
    	if( symbGetIsAccessed( s ) = FALSE ) then
			'' not public?
    	    if( symbIsPublic( s ) = FALSE ) then
    	    	return
    	    end if
		end if

		hEmitDataHeader( )
		astTypeIniFlush( s->var_.initree, _
						 s, _
						 AST_INIOPT_ISINI or AST_INIOPT_ISSTATIC )

		return
	end if

    '' extern or dynamic (for the latter, only the array descriptor is emitted)?
	if( (s->attrib and (FB_SYMBATTRIB_EXTERN or _
			   			FB_SYMBATTRIB_DYNAMIC)) <> 0 ) then
		return
	end if

    '' a string or array descriptor?
	if( symbGetLen( s ) <= 0 ) then
		return
	end if

	hEmitVarBss( s )

end sub

'':::::
private sub hClearLocalsAtt _
	( _
		byval bytestoclear as integer, _
		byval baseoffset as integer _
	) static

	dim as integer i
    dim as string lname

	if( bytestoclear = 0 ) then
		exit sub
	end if

	if( env.clopt.cputype >= FB_CPUTYPE_686 ) then
		if( cunsg(bytestoclear) \ 8 > 7 ) then

	    	if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EDI ) = FALSE ) then
    			hPUSHAtt( "%edi" )
    		end if

			outp( "leal -" & baseoffset + bytestoclear & "(%ebp), %edi" )
			outp( "movl $" & cunsg(bytestoclear) \ 8 & ", %ecx" )
			outp( "pxor %mm0, %mm0" )
		    lname = *hMakeTmpStr( )
		    hLABEL( lname )
			outp( "movq %mm0, (%edi)" )
			outp( "addl $8, %edi" )
			outp( "decl %ecx" )
			outp( "jnz " + lname )
			outp( "emms" )

    		if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EDI ) = FALSE ) then
    			hPOPAtt( "%edi" )
    		end if

		elseif( cunsg(bytestoclear) \ 8 > 0 ) then
			outp( "pxor %mm0, %mm0" )
			for i = cunsg(bytestoclear) \ 8 to 1 step -1
				outp( "movq %mm0, -" & ( i*8 ) & "(%ebp)" )
			next
			outp( "emms" )

		end if

		if( bytestoclear and 4 ) then
			outp( "movl $0, -" & baseoffset + bytestoclear & "(%ebp)" )
		end if

		exit sub
	end if

	if( cunsg(bytestoclear) \ 4 > 6 ) then

    	if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EDI ) = FALSE ) then
   			hPUSHAtt( "%edi" )
   		end if

		outp( "leal -" & baseoffset + bytestoclear & "(%ebp), %edi" )
		outp( "movl $" & cunsg(bytestoclear) \ 4 & ", %ecx" )
		outp( "xorl %eax, %eax" )
		outp( "rep" )
		outp( "stosl" )

   		if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EDI ) = FALSE ) then
   			hPOPAtt( "%edi" )
   		end if

	else
		for i = cunsg(bytestoclear) \ 4 to 1 step -1
			 outp( "movl $0, -" & baseoffset + ( i*4 ) & "(%ebp)" )
		next
	end if

end sub

'':::::
private sub hCreateFrameAtt _
	( _
		byval proc as FBSYMBOL ptr _
	) static

    dim as integer bytestoalloc, bytestoclear
	dim as zstring ptr lprof

	' No frame for naked functions
	if (proc->attrib and FB_SYMBATTRIB_NAKED) = 0 then

		bytestoalloc = ((proc->proc.ext->stk.localmax - EMIT_LOCSTART) + 3) and (not 3)

		if( (bytestoalloc <> 0) or _
			(proc->proc.ext->stk.argofs <> EMIT_ARGSTART) or _
			symbGetIsMainProc( proc ) or _
			env.clopt.debug or _
			env.clopt.profile ) then

			hPUSHAtt( "%ebp" )
			outp( "movl %esp, %ebp" )

			if( symbGetIsMainProc( proc ) ) then
				outp( "andl $0xFFFFFFF0, %esp" )
			end if

			if( bytestoalloc > 0 ) then
				outp( "subl $" + str( bytestoalloc ) + ", %esp" )
			end if
		end if

		if( env.clopt.target = FB_COMPTARGET_DOS ) then
			if( env.clopt.profile ) then
				lprof = hMakeProfileLabelName()

				outEx(".section .data" + NEWLINE )
				outEx( ".balign 4" + NEWLINE )
				outEx( "." + *lprof + ":" + NEWLINE )
				outp( ".long 0" )
				outEx( ".section .text" + NEWLINE )
				outp( "movl $." + *lprof + ", %edx" )
				outp( "call _mcount" )
			end if
		end if

		if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EBX ) ) then
			hPUSHAtt( "%ebx" )
		end if
		if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_ESI ) ) then
			hPUSHAtt( "%esi" )
		end if
		if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EDI ) ) then
			hPUSHAtt( "%edi" )
		end if

	end if
		
end sub

''::::
private sub hDestroyFrameAtt _
	( _
		byval proc as FBSYMBOL ptr, _
		byval bytestopop as integer _
	) static

	' don't do anything for naked functions, except the .size at the end
	if (proc->attrib and FB_SYMBATTRIB_NAKED) = 0 then

    	dim as integer bytestoalloc

    	bytestoalloc = ((proc->proc.ext->stk.localmax - EMIT_LOCSTART) + 3) and (not 3)

    	if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EDI ) ) then
    		hPOPAtt( "%edi" )
    	end if
    	if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_ESI ) ) then
    		hPOPAtt( "%esi" )
    	end if
    	if( EMIT_REGISUSED( FB_DATACLASS_INTEGER, EMIT_REG_EBX ) ) then
    		hPOPAtt( "%ebx" )
    	end if

    	if( (bytestoalloc <> 0) or _
    		(proc->proc.ext->stk.argofs <> EMIT_ARGSTART) or _
        	symbGetIsMainProc( proc ) or _
        	env.clopt.debug or _
			env.clopt.profile ) then
    		outp( "movl %ebp, %esp" )
    		hPOPAtt( "%ebp" )
    	end if

    	if( bytestopop > 0 ) then
    		outp( "ret $" + str( bytestopop ) )
    	else
    		outp( "ret" )
    	end if

	end if

	if( env.clopt.target = FB_COMPTARGET_LINUX ) then
    	outEx( ".size " + *symbGetMangledName( proc ) + ", .-" + *symbGetMangledName( proc ) + NEWLINE )
	end if

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' implementation
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttLIT _
	( _
		byval s as zstring ptr )
    dim ostr as string

    ostr = *s + NEWLINE
	outEX( ostr )

end sub

'':::::
private sub _emitAttALIGN _
	( _
		byval vreg as IRVREG ptr _
	) static

    dim ostr as string

    ostr = ".balign " + str( vreg->value.int )
	outp( ostr )

end sub

'':::::
private sub _emitAttSTKALIGN _
	( _
		byval vreg as IRVREG ptr _
	) static

    dim ostr as string

	if( vreg->value.int > 0 ) then
		ostr = "subl $" + str( vreg->value.int ) + ", %esp"
	else
		ostr = "addl $" + str( -vreg->value.int ) + ", %esp"
	end if

	outp( ostr )

end sub

'':::::
private sub _emitAttJMPTB _
	( _
		byval op as AST_JMPTB_OP, _
		byval dtype as integer, _
		byval label as zstring ptr _
	) static

    dim ostr as string

	select case op
	case AST_JMPTB_LABEL
		ostr = *emit.vtbl.getTypeString( dtype ) + " " + *label
		outp( ostr )
	case AST_JMPTB_BEGIN
		ostr = *label
		ostr += ":" + NEWLINE
		outEx( ostr )
	end select

end sub

'':::::
private sub _emitAttCALL _
	( _
		byval unused as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval bytestopop as integer _
	) static

    dim ostr as string

	ostr = "call "
	ostr += *symbGetMangledName( label )
	outp( ostr )

    if( bytestopop <> 0 ) then
		ostr = "addl $" +str( bytestopop ) + ", %esp"
    	outp( ostr )
    end if

end sub

'':::::
private sub _emitAttCALLPTR _
	( _
		byval svreg as IRVREG ptr, _
		byval unused as FBSYMBOL ptr, _
		byval bytestopop as integer _
	) static

    dim src as string
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	'' apparently att syntax gives a warning if there's no *.
	ostr = "call *" + src
	outp( ostr )

    if( bytestopop <> 0 ) then
		ostr = "addl $" +str( bytestopop ) + ", %esp"
    	outp( ostr )
    end if

end sub

'':::::
private sub _emitAttBRANCH _
	( _
		byval unused as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval op as integer _
	) static

    dim ostr as string

	select case as const op
	case AST_OP_JLE
		ostr = "jle "
	case AST_OP_JGE
		ostr = "jge "
	case AST_OP_JLT
		ostr = "jl "
	case AST_OP_JGT
		ostr = "jg "
	case AST_OP_JEQ
		ostr = "je "
	case AST_OP_JNE
		ostr = "jne "
	end select

	ostr += *symbGetMangledName( label )
	outp( ostr )

end sub

'':::::
private sub _emitAttJUMP _
	( _
		byval unused1 as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval unused2 as integer _
	) static

    dim ostr as string

	ostr = "jmp "
	ostr += *symbGetMangledName( label )
	outp( ostr )

end sub

'':::::
private sub _emitAttJUMPPTR _
	( _
		byval svreg as IRVREG ptr, _
		byval unused1 as FBSYMBOL ptr, _
		byval unused2 as integer _
	) static

    dim src as string
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	ostr = "jmp *" + src
	outp( ostr )

end sub

'':::::
private sub _emitAttRET _
	( _
		byval vreg as IRVREG ptr _
	) static

    dim ostr as string

    ostr = "ret $"
	ostr += str( vreg->value.int )
    outp( ostr )

end sub

'':::::
private sub _emitAttPUBLIC _
	( _
		byval label as FBSYMBOL ptr _
	) static

    dim ostr as string

	ostr = NEWLINE + ".globl "
	ostr += *symbGetMangledName( label )
	ostr += NEWLINE
	outEx( ostr )

end sub

'':::::
private sub _emitAttLABEL _
	( _
		byval label as FBSYMBOL ptr _
	) static

    dim ostr as string

	ostr = *symbGetMangledName( label )
	ostr += ":" + NEWLINE
	outEx( ostr )

end sub

'':::::
private sub _emitAttNOP _
	( _
		_
	) static

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' store
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub hULONG2DBL _
	( _
		byval svreg as IRVREG ptr _
	) static

	dim as string label, aux, ostr

	label = *hMakeTmpStr( )

	hPrepOperandAtt( svreg, aux, FB_DATATYPE_INTEGER, 0, TRUE )
	ostr = "cmpl $0, " + aux

	outp ostr
	ostr = "jns " + label
	outp ostr
	hPUSHAtt( "$0x403f" )
	hPUSHAtt( "$0x80000000" )
	hPUSHAtt( "$0" )
	outp "fldt (%esp)"
	outp "addl $12, %esp"
	outp "faddp"
	hLABEL( label )

end sub

'':::::
private sub _emitAttSTORL2L _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst1, dst2, src1, src2, ostr

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "movl " + src1 + COMMA + dst1
	outp ostr

	ostr = "movl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttSTORI2L _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst1, dst2, src1, ext, ostr
    dim sdsize as integer

	sdsize = symbGetDataSize( svreg->dtype )

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	hPrepOperandAtt( svreg, src1 )

	'' immediate?
	if( svreg->typ = IR_VREGTYPE_IMM ) then
		hMOVAtt dst1, src1

		'' negative?
		if( symbIsSigned( svreg->dtype ) and (svreg->value.int and &h80000000) ) then
			hMOVAtt dst2, "$-1"
		else
			hMOVAtt dst2, "$0"
		end if

		exit sub
	end if

	''
	if( sdsize < FB_INTEGERSIZE ) then
		ext = *hGetRegNameAtt( FB_DATATYPE_INTEGER, svreg->reg )

		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "movs"
		else
			ostr = "movz"
		end if
		ostr += dtsuffixTB(svreg->dtype).mname + "l "
		ostr += src1 + COMMA + ext
		outp ostr

	else
		ext = src1
	end if

	ostr = "movql " + ext + COMMA + dst1
	outp ostr

	if( symbIsSigned( svreg->dtype ) ) then

		hPUSHAtt ext

		ostr = "sarl " + ext + ", $31"
		outp ostr

		ostr = "movl " + ext + COMMA + dst2
		outp ostr

		hPOPAtt ext

	else
		ostr = "movl $0, " + dst2
		outp ostr
	end if

end sub


'':::::
private sub _emitAttSTORF2L _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )

	'' signed?
	if( symbIsSigned( dvreg->dtype ) ) then
		ostr = "fistp" + dtsuffixTB(dvreg->dtype).mname + " " + dst
		outp ostr

	end if

end sub


'':::::
private sub _emitAttSTORI2I _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ddsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ddsize = symbGetDataSize( dvreg->dtype )

	if( ddsize = 1 ) then
		if( svreg->typ = IR_VREGTYPE_IMM ) then
			ddsize = 4
		end if
	end if

	'' dst size = src size
	if( (svreg->typ = IR_VREGTYPE_IMM) or _
		(dvreg->dtype = svreg->dtype) or _
		(symbMaxDataType( dvreg->dtype, svreg->dtype ) = FB_DATATYPE_INVALID) ) then

		ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
		outp ostr

	'' sizes are different..
	else
    	dim as string aux

		aux = *hGetRegNameAtt( dvreg->dtype, svreg->reg )

		'' dst size > src size
		if( dvreg->dtype > svreg->dtype ) then
			if( symbIsSigned( svreg->dtype ) ) then
				ostr = "movs"
			else
				ostr = "movz"
			end if
			ostr += dtsuffixTB(svreg->dtype).mname + dtsuffixTB(dvreg->dtype).mname + " "
			ostr += src + COMMA + aux
			outp ostr

			ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + aux + COMMA + dst
			outp ostr

		'' dst size < src size
		else
            '' handle DI/SI as source stored into a byte destine
            dim as integer is_disi

            is_disi = FALSE
            if( ddsize = 1 ) then
            	if( svreg->typ = IR_VREGTYPE_REG ) then
            		is_disi = (svreg->reg = EMIT_REG_ESI) or (svreg->reg = EMIT_REG_EDI)
            	end if
            end if

			if( is_disi ) then
    			dim as string aux8
    			dim as integer reg, isfree

				reg = hFindRegNotInVreg( dvreg, TRUE )

				aux8 = *hGetRegNameAtt( FB_DATATYPE_BYTE, reg )
				aux = *hGetRegNameAtt( svreg->dtype, reg )

				isfree = hIsRegFree(FB_DATACLASS_INTEGER, reg )
				if( isfree = FALSE ) then
					hPUSHAtt aux
				end if

				ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + aux
				outp ostr

				ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + aux8 + COMMA + dst
				outp ostr

				if( isfree = FALSE ) then
					hPOPAtt aux
				end if

			else
				ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + aux + COMMA + dst
				outp ostr
			end if
		end if
	end if

end sub

'':::::
private sub _emitAttSTORL2I _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	'' been too complex due the SI/DI crap, leave it to I2I
	_emitAttSTORI2I( dvreg, svreg )

end sub


'':::::
private sub _emitAttSTORF2I _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ddsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ddsize = symbGetDataSize( dvreg->dtype )

	'' byte destine? damn..
	if( ddsize = 1 ) then

		outp "subl $4, %esp"
		outp "fistpl (%esp)"

        '' destine is a reg?
        if( dvreg->typ = IR_VREGTYPE_REG ) then

			ostr = "movb (%esp), " + dst
			outp ostr
           	outp "addl $4, %esp"

		'' destine is a var/idx/ptr
		else
            dim as integer reg, isfree
            dim as string aux, aux8

			reg = hFindRegNotInVreg( dvreg, TRUE )

			aux8 = *hGetRegNameAtt( FB_DATATYPE_BYTE, reg )
			aux  = *hGetRegNameAtt( FB_DATATYPE_INTEGER, reg )

			isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

			if( isfree = FALSE ) then
				hXCHGAtt aux, "(%esp)"
			else
				ostr = "movb (%esp), " + aux8
				outp ostr
			end if

			hMOVAtt dst, aux8

			if( isfree = FALSE ) then
				hPOPAtt aux
			else
				outp "add $4, %esp"
			end if

		end if

	else
		'' signed?
		if( symbIsSigned( dvreg->dtype ) ) then
			ostr = "fistpl " + dst
			outp ostr

		'' unsigned.. try a bigger type
		else
			'' uint?
			if( ddsize = FB_INTEGERSIZE ) then
				outp "subl $8, %esp"
				outp "fistpq (%esp)"
				hPOPAtt dst
				outp  "addl $4, %esp"

			'' ushort..
			else
				outp "subl $4, %esp"
				outp "fistpl (%esp)"
				hPOPAtt dst
				outp  "add $2, %esp"
			end if
		end if

	end if

end sub



'':::::
private sub _emitAttSTORL2F _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src, aux
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	if( (svreg->typ = IR_VREGTYPE_REG) or (svreg->typ = IR_VREGTYPE_IMM) ) then

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then

			hPrepOperandAtt64( svreg, src, aux )

			hPUSHAtt( aux )
			hPUSHAtt( src )

			ostr = "fild" + dtsuffixTB(dvreg->dtype).mname + " (%esp)"
			outp ostr

			outp "addl $8, %esp"

		'' unsigned..
		else
			hPrepOperandAtt64( svreg, src, aux )
			hPUSHAtt aux
			hPUSHAtt src
			outp "fildq (%esp)"
			outp "addl $8, %esp"
			hULONG2DBL( svreg )

		end if

	'' not a reg or imm
	else
		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr

		'' unsigned, try a bigger type..
		else
			ostr = "fild" + dtsuffixTB(dvreg->dtype).mname + " " + src
			outp ostr
			hULONG2DBL( svreg )

		end if
	end if

	ostr = "fstp" + dtsuffixTB(dvreg->dtype).mfl + " " + dst
	outp ostr

end sub



'':::::
private sub _emitAttSTORI2F _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ddsize, sdsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ddsize = symbGetDataSize( dvreg->dtype )
	sdsize = symbGetDataSize( svreg->dtype )

	'' byte source? damn..
	if( sdsize = 1 ) then
    	dim as string aux
    	dim as integer reg, isfree

		reg = hFindRegNotInVreg( svreg )

		aux = *hGetRegNameAtt( FB_DATATYPE_INTEGER, reg )

		isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

		if( isfree = FALSE ) then
			hPUSHAtt aux
		end if

		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "movs"
		else
			ostr = "movz"
		end if
		ostr += dtsuffixTB(svreg->dtype).mname + dtsuffixTB(dvreg->dtype).mname + " "
		ostr += aux + COMMA + src
		outp ostr

		hPUSHAtt aux
		outp "fildl (%esp)"
		outp "addl $4, %esp"

		if( isfree = FALSE ) then
			hPOPAtt aux
		end if

		ostr = "fstp" + dtsuffixTB(dvreg->dtype).mfl + " " + dst
		outp ostr

		exit sub
	end if

	''
	if( (svreg->typ = IR_VREGTYPE_REG) or (svreg->typ = IR_VREGTYPE_IMM) ) then

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then

			'' not an integer? make it
			if( (svreg->typ = IR_VREGTYPE_REG) and (sdsize < FB_INTEGERSIZE) ) then
				src = *hGetRegNameAtt( FB_DATATYPE_INTEGER, svreg->reg )
			end if

			hPUSHAtt src

			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " (%esp)"
			outp ostr

			outp "addl $4, %esp"

		'' unsigned..
		else

			'' uint..
			if( sdsize = FB_INTEGERSIZE ) then
				hPUSHAtt "$0"
				hPUSHAtt src
				outp "fildq (%esp)"
				outp "addl $8, %esp"

			'' ushort..
			else
				if( svreg->typ <> IR_VREGTYPE_IMM ) then
					hPUSHAtt "$0"
				end if

				hPUSHAtt src
				outp "fildl (%esp)"

				if( svreg->typ <> IR_VREGTYPE_IMM ) then
					outp "addl $6, %esp"
				else
					outp "addl $4, %esp"
				end if
			end if

		end if

	'' not a reg or imm
	else

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr

		'' unsigned, try a bigger type..
		else
			'' uint..
			if( sdsize = FB_INTEGERSIZE ) then
				hPUSHAtt "$0"
				hPUSHAtt src
				outp "fildq (%esp)"
				outp "addl $8, %esp"

			'' ushort..
			else
				hPUSHAtt "$0"
				hPUSHAtt src
				outp "fildl (%esp)"
				outp "addl $6, %esp"
			end if

		end if
	end if

	ostr = "fstp" + dtsuffixTB(dvreg->dtype).mfl + " " + dst
	outp ostr

end sub



'':::::
private sub _emitAttSTORF2F _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ddsize, sdsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ddsize = symbGetDataSize( dvreg->dtype )
	sdsize = symbGetDataSize( svreg->dtype )

	'' on fpu stack?
	if( svreg->typ = IR_VREGTYPE_REG ) then
		ostr = "fstp" + dtsuffixTB(dvreg->dtype).mfl + " " + dst
		outp ostr

	else
		'' same size? just copy..
		if( sdsize = ddsize ) then

			hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER, 0 )
			ostr = "pushl " + src
			outp ostr

			if( sdsize > 4 ) then
				hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER, 4 )
				ostr = "pushl " + src
				outp ostr

				hPrepOperandAtt( dvreg, dst, FB_DATATYPE_INTEGER, 4 )
				ostr = "popl " + dst
				outp ostr
			end if

			hPrepOperandAtt( dvreg, dst, FB_DATATYPE_INTEGER, 0 )
			ostr = "popl " + dst
			outp ostr

		'' diff sizes, convert..
		else
			ostr = "fld" + dtsuffixTB(svreg->dtype).mfl + " " + src
			outp ostr

			ostr = "fstp" + dtsuffixTB(dvreg->dtype).mfl + " " + dst
			outp ostr
		end if
	end if

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' load
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttLOADL2L _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst1, dst2, src1, src2
    dim as string ostr

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "movl " + src1 + COMMA + dst1
	outp ostr

	ostr = "movl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttLOADI2L _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst1, dst2, src1
    dim as integer sdsize
    dim as string ostr

	sdsize = symbGetDataSize( svreg->dtype )

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	hPrepOperandAtt( svreg, src1 )

	'' immediate?
	if( svreg->typ = IR_VREGTYPE_IMM ) then

        hMOVAtt dst1, src1

		'' negative?
		if( symbIsSigned( svreg->dtype ) and (svreg->value.int and &h80000000) ) then
			hMOVAtt dst2, "$-1"
		else
			hMOVAtt dst2, "$0"
		end if

		exit sub
	end if

	''
	if( symbIsSigned( svreg->dtype ) ) then

		if( sdsize < FB_INTEGERSIZE ) then
			ostr = "movs"
			ostr += dtsuffixTB(svreg->dtype).mname + "l "
			ostr += src1 + COMMA + dst1
			outp ostr
		else
			hMOVAtt dst1, src1
		end if

		hMOVAtt dst2, dst1

		ostr = "sarl $31, " + dst2
		outp ostr

	else

		if( sdsize < FB_INTEGERSIZE ) then
			ostr = "movz"
			ostr += dtsuffixTB(svreg->dtype).mname + "l "
			ostr += src1 + COMMA + dst1
			outp ostr
		else
			hMOVAtt dst1, src1
		end if

		hMOVAtt dst2, "$0"

	end if

end sub



'':::::
private sub _emitAttLOADF2L _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src, aux
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	if( svreg->typ <> IR_VREGTYPE_REG ) then
		ostr = "fld" + dtsuffixTB(svreg->dtype).mfl + " " + src
		outp ostr
	end if

	hPrepOperandAtt64( dvreg, dst, aux )

	'' signed?
	'' (handle ULONGINT here too - workaround for #2082801)
	if( symbIsSigned( dvreg->dtype ) orelse (dvreg->dtype = FB_DATATYPE_ULONGINT) ) then

		outp "subl $8, %esp"

		ostr = "fistp" + dtsuffixTB(svreg->dtype).mfl + " (%esp)"
		outp ostr

	'' unsigned.. try a bigger type
	else
		outp "fld %st(0)"
		'' UWtype hi = (UWtype)(a / Wtype_MAXp1_F)
		outp "pushl $0x4f800000"
		outp "fdivl (%esp)"
		outp "fistpl (%esp)"
		'' UWtype lo = (UWtype)(a - ((DFtype)hi) * Wtype_MAXp1_F)
		outp "fildl (%esp)"
		outp "pushl $0x4f800000"
		outp "fmull (%esp)"
		outp "fsubp"
		outp "fistpl (%esp)"
		'' ((UDWtype) hi << W_TYPE_SIZE) | lo
	end if

	hPOPAtt( dst )
	hPOPAtt( aux )

end sub

'':::::
private sub _emitAttLOADI2I _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ddsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )


	ddsize = symbGetDataSize( dvreg->dtype )

	if( ddsize = 1 ) then
		if( svreg->typ = IR_VREGTYPE_IMM ) then
			ddsize = 4
		end if
	end if

	'' dst size = src size
	if( (dvreg->dtype = svreg->dtype) or _
		(symbMaxDataType( dvreg->dtype, svreg->dtype ) = FB_DATATYPE_INVALID) ) then

		ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
		outp ostr



	else
		'' dst size > src size
		if( dvreg->dtype > svreg->dtype ) then
			if( symbIsSigned( svreg->dtype ) ) then
				ostr = "movs"
			else
				ostr = "movz"
			end if
			ostr += dtsuffixTB(svreg->dtype).mname + dtsuffixTB(dvreg->dtype).mname + " "
			ostr += src + COMMA + dst
			outp ostr

		'' dst dize < src size
		else
			'' is src a reg too?
			if( svreg->typ = IR_VREGTYPE_REG ) then
				'' not the same?
				if( svreg->reg <> dvreg->reg ) then
					dim as string aux
					dim as integer dtype

					dtype = dvreg->dtype

					'' handle [E]DI/[E]SI source loaded to a byte destine
					if( ddsize = 1 ) then
						if( (svreg->reg = EMIT_REG_ESI) or _
							(svreg->reg = EMIT_REG_EDI) ) then

							dtype = FB_DATATYPE_INTEGER
							dst = *hGetRegNameAtt( dtype, dvreg->reg )
						end if
					end if

					aux = *hGetRegNameAtt( dtype, svreg->reg )
					ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + aux + COMMA + dst
					outp ostr
				end if

			'' src is not a reg
			else
				hPrepOperandAtt( svreg, src, dvreg->dtype )

				ostr = "mov" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
				outp ostr
			end if
		end if
	end if

end sub

'':::::
private sub _emitAttLOADL2I _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	'' been too complex due the SI/DI crap, leave it to I2I
	_emitAttLOADI2I( dvreg, svreg )

end sub


'':::::
private sub _emitAttLOADF2I _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ddsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ddsize = symbGetDataSize( dvreg->dtype )

	if( svreg->typ <> IR_VREGTYPE_REG ) then
		ostr = "fld" + dtsuffixTB(dvreg->dtype).mfl + " " + src
		outp ostr
	end if

	'' byte destine? damn..
	if( ddsize = 1 ) then

		outp "subl $4, %esp"
        outp "fistpl (%esp)"

    	'' destine is a reg
    	if( dvreg->typ = IR_VREGTYPE_REG ) then
			ostr = "movb (%esp), " + dst
			outp ostr
			outp "addl $4, %esp"

		'' destine is a var/idx/ptr
        else
    		dim as string aux, aux8
    		dim as integer reg, isfree

			reg = hFindRegNotInVreg( dvreg, TRUE )

			aux8 = *hGetRegNameAtt( FB_DATATYPE_BYTE, reg )
			aux  = *hGetRegNameAtt( FB_DATATYPE_INTEGER, reg )

			isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

			if( isfree = FALSE ) then
				hXCHGAtt aux, "(%esp)"
			else
				ostr = "movb (%esp), " + aux8
				outp ostr
			end if

			hMOVAtt dst, aux8

			if( isfree = FALSE ) then
				hPOPAtt aux
			else
				outp "addl $4, %esp"
			end if

        end if

	else

		'' signed?
		if( symbIsSigned( dvreg->dtype ) ) then

			outp "subl $4, %esp"

			ostr = "fistpl (%esp)"
			outp ostr

			'' not an integer? make it
			if( ddsize < FB_INTEGERSIZE ) then
				dst = *hGetRegNameAtt( FB_DATATYPE_INTEGER, dvreg->reg )
			end if

			hPOPAtt dst

		'' unsigned.. try a bigger type
		else

			'' uint?
			if( ddsize = FB_INTEGERSIZE ) then
				outp "subl $8, %esp"
				outp "fistpq (%esp)"
				hPOPAtt dst
				outp  "addl $4, %esp"

			'' ushort..
			else
				outp "subl $4, %esp"
				outp "fistpl (%esp)"
				hPOPAtt dst
				outp  "addl $2, %esp"
			end if

		end if

	end if

end sub


'':::::
private sub _emitAttLOADL2F _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src, aux
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	if( (svreg->typ = IR_VREGTYPE_REG) or (svreg->typ = IR_VREGTYPE_IMM) ) then

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then

			hPrepOperandAtt64( svreg, src, aux )

			hPUSHAtt( aux )
			hPUSHAtt( src )

			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " (%esp)"
			outp ostr

			outp "addl $8, %esp"

		'' unsigned, try a bigger type..
		else

			hPrepOperandAtt64( svreg, src, aux )
			hPUSHAtt aux
			hPUSHAtt src
			outp "fildq (%esp)"
			outp "addl $8, %esp"
			hULONG2DBL( svreg )

		end if

	'' not a reg or imm
	else

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr

		'' unsigned, try a bigger type..
		else
			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr
			hULONG2DBL( svreg )

		end if

	end if

end sub



'':::::
private sub _emitAttLOADI2F _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer sdsize
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

    sdsize = symbGetDataSize( svreg->dtype )

	'' byte source? damn..
	if( sdsize = 1 ) then
    	dim as string aux
    	dim as integer isfree, reg

		reg = hFindRegNotInVreg( svreg )

		aux = *hGetRegNameAtt( FB_DATATYPE_INTEGER, reg )

		isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

		if( isfree = FALSE ) then
			hPUSHAtt aux
		end if

		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "movs"
			ostr += dtsuffixTB(svreg->dtype).mname + "l "
			ostr += src + COMMA + aux
			outp ostr
		else
			ostr = "movz"
			ostr += dtsuffixTB(svreg->dtype).mname + "l "
			ostr += src + COMMA + aux
			outp ostr
		end if

		hPUSHAtt aux
		outp "fildl (%esp)"
		outp "addl $4, %esp"

		if( isfree = FALSE ) then
			hPOPAtt aux
		end if

		exit sub
	end if

	''
	if( (svreg->typ = IR_VREGTYPE_REG) or (svreg->typ = IR_VREGTYPE_IMM) ) then

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then

			'' not an integer? make it
			if( (svreg->typ = IR_VREGTYPE_REG) and (sdsize < FB_INTEGERSIZE) ) then
				src = *hGetRegNameAtt( FB_DATATYPE_INTEGER, svreg->reg )
			end if

			hPUSHAtt src

			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " (%esp)"
			outp ostr

			outp "addl $4, %esp"

		'' unsigned, try a bigger type..
		else

			'' uint?
			if( sdsize = FB_INTEGERSIZE ) then
				hPUSHAtt "$0"
				hPUSHAtt src
				outp "fildq (%esp)"
				outp "addl $8, %esp"

			'' ushort..
			else
				if( svreg->typ <> IR_VREGTYPE_IMM ) then
					hPUSHAtt "$0"
				end if

				hPUSHAtt src
				outp "fildl (%esp)"

				if( svreg->typ <> IR_VREGTYPE_IMM ) then
					outp "addl $6, %esp"
				else
					outp "addl $4, %esp"
				end if
			end if

		end if

	'' not a reg or imm
	else

		'' signed?
		if( symbIsSigned( svreg->dtype ) ) then
			ostr = "fild" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr

		'' unsigned, try a bigger type..
		else
			'' uint..
			if( sdsize = FB_INTEGERSIZE ) then
				hPUSHAtt "$0"
				hPUSHAtt src
				outp "fildq (%esp)"
				outp "addl $8, %esp"

			'' ushort..
			else
				hPUSHAtt "$0"
				hPUSHAtt src
				outp "fildl (%esp)"
				outp "addl $6, %esp"
			end if
		end if

	end if

end sub



'':::::
private sub _emitAttLOADF2F _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string src
    dim as string ostr

	hPrepOperandAtt( svreg, src )

	ostr = "fld" + dtsuffixTB(svreg->dtype).mfl + " " + src
	outp ostr

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' binary ops
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttMOVL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst1, dst2, src1, src2, ostr

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "movl " + src1 + COMMA + dst1
	outp ostr

	ostr = "movl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttMOVI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src, ostr

	'' byte? handle SI, DI used as bytes..
	if( symbGetDataSize( dvreg->dtype ) = 1 ) then
		'' MOV is only used when both operands are registers
		dst = *hGetRegNameAtt( FB_DATATYPE_INTEGER, dvreg->reg )
		src = *hGetRegNameAtt( FB_DATATYPE_INTEGER, svreg->reg )
	else
		hPrepOperandAtt( dvreg, dst )
		hPrepOperandAtt( svreg, src )
	end if

	ostr = "movl " + src + COMMA + dst
	outp ostr

end sub


'':::::
private sub _emitAttMOVF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	'' do nothing, both are regs

end sub

'':::::
private sub _emitAttADDL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "addl " + src1 + COMMA + dst1
	outp ostr

	ostr = "adcl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttADDI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim doinc as integer, dodec as integer
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	doinc = FALSE
	dodec = FALSE
	if( svreg->typ = IR_VREGTYPE_IMM ) then
		select case svreg->value.int
		case 1
			doinc = TRUE
		case -1
			dodec = TRUE
		end select
	end if

	if( doinc ) then
		ostr = "inc" + dtsuffixTB(dvreg->dtype).mname + " " + dst
		outp ostr
	elseif( dodec ) then
		ostr = "dec" + dtsuffixTB(dvreg->dtype).mname + " " + dst
		outp ostr
	else
		ostr = "add" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
		outp ostr
	end if

end sub



'':::::
private sub _emitAttADDF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim src as string
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	if( svreg->typ = IR_VREGTYPE_REG ) then
		ostr = "faddp"
		outp ostr
	else
		if( symbGetDataClass( svreg->dtype ) = FB_DATACLASS_FPOINT ) then
			ostr = "fadd" + dtsuffixTB(svreg->dtype).mfl + " " + src
			outp ostr
		else
			ostr = "fiadd" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr
		end if
	end if

end sub

'':::::
private sub _emitAttSUBL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "subl " + src1 + COMMA + dst1
	outp ostr

	ostr = "sbbl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttSUBI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim doinc as integer, dodec as integer
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	doinc = FALSE
	dodec = FALSE
	if( svreg->typ = IR_VREGTYPE_IMM ) then
		select case svreg->value.int
		case 1
			dodec = TRUE
		case -1
			doinc = TRUE
		end select
	end if

	if( dodec ) then
		ostr = "dec" + dtsuffixTB(dvreg->dtype).mname + " " + dst
		outp ostr
	elseif( doinc ) then
		ostr = "inc" + dtsuffixTB(dvreg->dtype).mname + " " + dst
		outp ostr
	else
		ostr = "sub" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
		outp ostr
	end if

end sub



'':::::
private sub _emitAttSUBF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim src as string
    dim doinc as integer, dodec as integer
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	if( svreg->typ = IR_VREGTYPE_REG ) then
		outp "fsubp"
	else
		if( symbGetDataClass( svreg->dtype ) = FB_DATACLASS_FPOINT ) then
			ostr = "fsub" + dtsuffixTB(svreg->dtype).mfl + " " + src
			outp ostr
		else
			ostr = "fisub" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr
		end if
	end if

end sub

'':::::
private sub _emitAttMULI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim eaxfree as integer, edxfree as integer
    dim edxtrashed as integer
    dim eaxinsource as integer, eaxindest as integer, edxindest as integer
    dim eax as string, edx as string
    dim ostr as string
    dim dst as string, src as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	eaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
	edxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )

	eaxinsource = hIsRegInVreg( svreg, EMIT_REG_EAX )
	eaxindest = hIsRegInVreg( dvreg, EMIT_REG_EAX )
	edxindest = hIsRegInVreg( dvreg, EMIT_REG_EDX )

    if( dtypeTB(dvreg->dtype).size = 4 ) then
    	eax = "%eax"
    	edx = "%edx"
    else
    	eax = "%ax"
    	edx = "%dx"
    end if

	if( (eaxinsource) or (svreg->typ = IR_VREGTYPE_IMM) ) then
		edxtrashed = TRUE
		if( edxindest ) then
			hPUSHAtt( "%edx" )
			if( dvreg->typ <> IR_VREGTYPE_REG ) then
				hPrepOperandAtt( dvreg, ostr, FB_DATATYPE_INTEGER )
				hPUSHAtt( ostr )
			end if
		elseif( edxfree = FALSE ) then
			hPUSHAtt( "%edx" )
		end if

		hMOVAtt( edx, src, dvreg->dtype )
		src = edx
	else
		edxtrashed = FALSE
	end if

	if( (eaxindest = FALSE) or (dvreg->typ <> IR_VREGTYPE_REG) ) then
		if( (edxindest) and (edxtrashed) ) then
			if( eaxfree = FALSE ) then
				outp "xchgl (%esp), %eax"
			else
				hPOPAtt "%eax"
			end if
		else
			if( eaxfree = FALSE ) then
				hPUSHAtt "%eax"
			end if
			hMOVAtt eax, dst, dvreg->dtype
		end if
	end if

	ostr = "mul" + dtsuffixTB(dvreg->dtype).mname + " " + src
	outp ostr

	if( eaxindest = FALSE ) then
		if( edxindest and dvreg->typ <> IR_VREGTYPE_REG ) then
			hPOPAtt "%edx"					'' edx= tos (eax)
			outp "xchgl (%esp), %edx"		'' tos= edx; edx= dst
		end if

		hMOVAtt dst, eax, dvreg->dtype

		if( eaxfree = FALSE ) then
			hPOPAtt "%eax"
		end if
	else
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hMOVAtt "%edx", "%eax"			'' edx= eax
			hPOPAtt "%eax"					'' restore eax
			hMOVAtt dst, edx, dvreg->dtype	'' [eax+...] = edx
		end if
	end if

	if( edxtrashed ) then
		if( (edxfree = FALSE) and (edxindest = FALSE) ) then
			hPOPAtt "%edx"
		end if
	end if

end sub

'':::::
private sub _emitAttMULL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim iseaxfree as integer, isedxfree as integer
    dim eaxindest as integer, edxindest as integer
    dim ofs as integer

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	iseaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
	isedxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )

	eaxindest = hIsRegInVreg( dvreg, EMIT_REG_EAX )
	edxindest = hIsRegInVreg( dvreg, EMIT_REG_EDX )

	hPUSHAtt( src2 )
	hPUSHAtt( src1 )
	hPUSHAtt( dst2 )
	hPUSHAtt( dst1 )

	ofs = 0

	if( edxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			ofs += 4
			hPUSHAtt( "%edx" )
		end if
	else
		if( isedxfree = FALSE ) then
			ofs += 4
			hPUSHAtt( "%edx" )
		end if
	end if

	if( eaxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			ofs += 4
			hPUSHAtt "%eax"
		end if
	else
		if( iseaxfree = FALSE ) then
			ofs += 4
			hPUSHAtt "%eax"
		end if
	end if

	'' res = low(dst) * low(src)
	outp "movl " + str( 0+ofs ) + "(%esp), %eax"
	outp "mull " + str( 8+ofs ) + "(%esp)"

	'' hres= low(dst) * high(src) + high(res)
	outp "xchgl " + str ( 0+ofs ) + "(%esp), %eax"

	outp "imull " + str ( 12+ofs ) + "(%esp), %eax"
	outp "addl %edx, %eax"

	'' hres += high(dst) * low(src)
	outp "movl " + str( 4+ofs ) + "(%esp), %edx"
	outp "imull " + str( 8+ofs ) + "(%esp), %edx"
	outp "addl %eax, %edx"
	outp "movl %edx, " + str( 4+ofs ) + "(%esp)"

	if( eaxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPOPAtt "%eax"
		end if
	else
		if( iseaxfree = FALSE ) then
			hPOPAtt "%eax"
		end if
	end if

	if( edxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPOPAtt "%edx"
		end if
	else
		if( isedxfree = FALSE ) then
			hPOPAtt "%edx"
		end if
	end if

	'' low(dst) = low(res)
	hPOPAtt dst1
	'' high(dst) = hres
	hPOPAtt dst2

	outp "addl $8, %esp"

	'' code:
	'' mov	eax, low(dst)
	'' mul	low(src)
	'' mov	ebx, low(dst)
	'' imul	ebx, high(src)
	'' add	ebx, edx
	'' mov	edx, high(dst)
	'' imul	edx, low(src)
	'' add	edx, ebx
	'' mov	low(dst), eax
	'' mov	high(dst), edx

end sub

'':::::
private sub _emitAttSMULI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim reg as integer, isfree as integer, rname as string
    dim ostr as string
    dim dst as string, src as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	if( dvreg->typ <> IR_VREGTYPE_REG ) then

		reg = hFindRegNotInVreg( svreg )
		rname = *hGetRegNameAtt( svreg->dtype, reg )

		isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

		if( isfree = FALSE ) then
			hPUSHAtt rname
		end if

		hMOVAtt rname, dst
		ostr = "imull " + src + COMMA + rname
		outp ostr
		hMOVAtt dst, rname

		if( isfree = FALSE ) then
			hPOPAtt rname
		end if

	else
		ostr = "imull " + src + COMMA + dst
		outp ostr
	end if

end sub



'':::::
private sub _emitAttMULF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim src as string
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	if( svreg->typ = IR_VREGTYPE_REG ) then
		outp "fmulp"
	else
		if( symbGetDataClass( svreg->dtype ) = FB_DATACLASS_FPOINT ) then
			ostr = "fmul" + dtsuffixTB(svreg->dtype).mfl + " " + src
			outp ostr
		else
			ostr = "fimul" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr
		end if
	end if

end sub



'':::::
private sub _emitAttDIVF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim src as string
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	if( svreg->typ = IR_VREGTYPE_REG ) then
		outp "fdivrp"
	else
		if( symbGetDataClass( svreg->dtype ) = FB_DATACLASS_FPOINT ) then
			ostr = "fdiv" + dtsuffixTB(svreg->dtype).mfl + " " + src
			outp ostr
		else
			ostr = "fidiv" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr
		end if
	end if

end sub

'':::::
private sub _emitAttDIVI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ecxtrashed
    dim as integer eaxfree, ecxfree, edxfree
    dim as integer eaxindest, ecxindest, edxindest
    dim as integer eaxinsource, edxinsource
    dim as string eax, ecx, edx
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

    if( dtypeTB(dvreg->dtype).size = 4 ) then
    	eax = "%eax"
    	ecx = "%ecx"
    	edx = "%edx"
    else
    	eax = "%ax"
    	ecx = "%cx"
    	edx = "%dx"
    end if

	ecxtrashed = FALSE

	eaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
	ecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )
	edxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )

	eaxinsource = hIsRegInVreg( svreg, EMIT_REG_EAX )
	edxinsource = hIsRegInVreg( svreg, EMIT_REG_EDX )
	eaxindest = hIsRegInVreg( dvreg, EMIT_REG_EAX )
	edxindest = hIsRegInVreg( dvreg, EMIT_REG_EDX )
	ecxindest = hIsRegInVreg( dvreg, EMIT_REG_ECX )

	if( (eaxinsource) or (edxinsource) or (svreg->typ = IR_VREGTYPE_IMM) ) then
		ecxtrashed = TRUE
		if( ecxindest ) then
			hPUSHAtt( "%ecx" )
			if( dvreg->typ <> IR_VREGTYPE_REG ) then
				hPrepOperandAtt( dvreg, ostr, FB_DATATYPE_INTEGER )
				hPUSHAtt( ostr )
			end if
		elseif( ecxfree = FALSE ) then
			hPUSHAtt( "%ecx" )
		end if
		hMOVAtt( ecx, src )
		src = ecx
	end if

	if( eaxindest = FALSE ) then
		if( (ecxindest) and (ecxtrashed) ) then
			if( eaxfree = FALSE ) then
				outp "xchgl (%esp), %eax"
			else
				hPOPAtt "%eax"
			end if
		else
			if( eaxfree = FALSE ) then
				hPUSHAtt "%eax"
			end if
			hMOVAtt eax, dst, dvreg->dtype
		end if

	else
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPUSHAtt "%eax"
			hMOVAtt eax, dst, dvreg->dtype
		end if
	end if

	if( edxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPUSHAtt "%edx"
		end if
	elseif( edxfree = FALSE ) then
		hPUSHAtt "%edx"
	end if

	if( symbIsSigned( dvreg->dtype ) ) then
		if( dtypeTB(dvreg->dtype).size = 4 ) then
			outp "cdq"
		else
			outp "cwd"
		end if

		ostr = "idiv" + dtsuffixTB(dvreg->dtype).mname + " " + src
		outp ostr

	else
		ostr = "xor" + dtsuffixTB(dvreg->dtype).mname + " " + edx + ", " + edx
		outp ostr

		ostr = "div" + dtsuffixTB(dvreg->dtype).mname + " " + src
		outp ostr
	end if

	if( edxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPOPAtt "%edx"
		end if
	elseif( edxfree = FALSE ) then
		hPOPAtt "%edx"
	end if

	if( eaxindest = FALSE ) then
		if( ecxindest and ecxtrashed ) then
			if( dvreg->typ <> IR_VREGTYPE_REG ) then
				if( eaxfree = FALSE ) then
					hPOPAtt "%ecx"					'' ecx= tos (eax)
					outp "xchgl %ecx, (%esp)"		'' tos= ecx; ecx= dst
				else
					hPOPAtt "%ecx"					'' ecx= tos (ecx)
				end if
			end if
		end if

		hMOVAtt dst, eax, dvreg->dtype

		if( eaxfree = FALSE ) then
			hPOPAtt "%eax"
		end if

	else
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			if( (ecxfree = FALSE) and (ecxtrashed = FALSE) ) then
				outp "xchgl (%esp), %ecx"		'' tos= ecx; ecx= dst
				outp "xchgl %eax, %ecx"			'' ecx= res; eax= dst
			else
				hMOVAtt "%ecx", "%eax"			'' ecx= eax
				hPOPAtt "%eax"					'' restore eax
			end if

			hMOVAtt dst, ecx, dvreg->dtype		'' [eax+...] = ecx

			if( (ecxfree = FALSE) and (ecxtrashed = FALSE) ) then
				hPOPAtt "%ecx"
			end if
		end if
	end if

	if( ecxtrashed ) then
		if( (ecxfree = FALSE) and (ecxindest = FALSE) ) then
			hPOPAtt "%ecx"
		end if
	end if

end sub

'':::::
private sub _emitAttMODI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string dst, src
    dim as integer ecxtrashed
    dim as integer eaxfree, ecxfree, edxfree
    dim as integer eaxindest, ecxindest, edxindest
    dim as integer eaxinsource, edxinsource
    dim as string eax, ecx, edx
    dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

    if( dtypeTB(dvreg->dtype).size = 4 ) then
    	eax = "%eax"
    	ecx = "%ecx"
    	edx = "%edx"
    else
    	eax = "%ax"
    	ecx = "%cx"
    	edx = "%dx"
    end if

	ecxtrashed = FALSE

	eaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
	ecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )
	edxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )

	eaxinsource = hIsRegInVreg( svreg, EMIT_REG_EAX )
	edxinsource = hIsRegInVreg( svreg, EMIT_REG_EDX )
	eaxindest = hIsRegInVreg( dvreg, EMIT_REG_EAX )
	edxindest = hIsRegInVreg( dvreg, EMIT_REG_EDX )
	ecxindest = hIsRegInVreg( dvreg, EMIT_REG_ECX )

	if( (eaxinsource) or (edxinsource) or (svreg->typ = IR_VREGTYPE_IMM) ) then
		ecxtrashed = TRUE
		if( ecxindest ) then
			hPUSHAtt( "%ecx" )
			if( dvreg->typ <> IR_VREGTYPE_REG ) then
				hPrepOperandAtt( dvreg, ostr, FB_DATATYPE_INTEGER )
				hPUSHAtt( ostr )
			end if
		elseif( ecxfree = FALSE ) then
			hPUSHAtt( "%ecx" )
		end if
		hMOVAtt( ecx, src, dvreg->dtype )
		src = ecx
	end if

	if( eaxindest = FALSE ) then
		if( (ecxindest) and (ecxtrashed) ) then
			if( eaxfree = FALSE ) then
				outp "xchgl (%esp), %eax"
			else
				hPOPAtt "%eax"
			end if
		else
			if( eaxfree = FALSE ) then
				hPUSHAtt "%eax"
			end if
			hMOVAtt eax, dst, dvreg->dtype
		end if

	else
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPUSHAtt "%eax"
			hMOVAtt eax, dst, dvreg->dtype
		end if
	end if

	if( edxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPUSHAtt "%edx"
		end if
	elseif( edxfree = FALSE ) then
		hPUSHAtt "%edx"
	end if

	if( symbIsSigned( dvreg->dtype ) ) then
		if( dtypeTB(dvreg->dtype).size = 4 ) then
			outp "cdq"
		else
			outp "cwd"
		end if

		ostr = "idiv" + dtsuffixTB(dvreg->dtype).mname + " " + src
		outp ostr

	else
		ostr = "xor" + dtsuffixTB(dvreg->dtype).mname + " " + edx + ", " + edx
		outp ostr

		ostr = "div" + dtsuffixTB(dvreg->dtype).mname + " " + src
		outp ostr
	end if

	hMOVAtt eax, edx, dvreg->dtype

	if( edxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPOPAtt "%edx"
		end if
	elseif( edxfree = FALSE ) then
		hPOPAtt "%edx"
	end if

	if( eaxindest = FALSE ) then
		if( ecxindest and ecxtrashed ) then
			if( dvreg->typ <> IR_VREGTYPE_REG ) then
				if( eaxfree = FALSE ) then
					hPOPAtt "%ecx"					'' ecx= tos (eax)
					outp "xchgl (%esp), %ecx"		'' tos= ecx; ecx= dst
				else
					hPOPAtt "%ecx"					'' ecx= tos (ecx)
				end if
			end if
		end if

		hMOVAtt dst, eax, dvreg->dtype

		if( eaxfree = FALSE ) then
			hPOPAtt "%eax"
		end if

	else
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			if( (ecxfree = FALSE) and (ecxtrashed = FALSE) ) then
				outp "xchgl (%esp), %ecx"		'' tos= ecx; ecx= dst
				outp "xchgl %eax, %ecx"			'' ecx= res; eax= dst
			else
				hMOVAtt "%ecx", "%eax"			'' ecx= eax
				hPOPAtt "%eax"					'' restore eax
			end if

			hMOVAtt dst, ecx, dvreg->dtype		'' [eax+...] = ecx

			if( (ecxfree = FALSE) and (ecxtrashed = FALSE) ) then
				hPOPAtt "%ecx"
			end if
		end if
	end if

	if( ecxtrashed ) then
		if( (ecxfree = FALSE) and (ecxindest = FALSE) ) then
			hPOPAtt "%ecx"
		end if
	end if

end sub

'':::::
private sub hSHIFTL _
	( _
		byval op as integer, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim as string dst1, dst2, src, label, mnemonic32, mnemonic64
	dim as integer tmpreg
	dim as string tmpregname
	dim as integer preserveeax, preserveecx, preserveebx, preserveedx
	dim as string a, b
	dim as IRVREG ptr av, bv
	dim as integer eaxindest, edxindest, ebxindest

	''
	if( op = AST_OP_SHL ) then
		'' x86 shl and sar are the same
		mnemonic32 = "shll "
		mnemonic64 = "shldl "
	else
		if( symbIsSigned( dvreg->dtype ) ) then
			mnemonic32 = "sarl "
		else
			mnemonic32 = "shrl "
		end if
		mnemonic64 = "shrdl "
	end if

	''
	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER )



	eaxindest = hIsRegInVreg( dvreg, EMIT_REG_EAX )
	edxindest = hIsRegInVreg( dvreg, EMIT_REG_EDX )
	ebxindest = hIsRegInVreg( dvreg, EMIT_REG_EBX )

	if( op = AST_OP_SHL ) then
		a = dst2
		av = dvreg->vaux
		b = dst1
		bv = dvreg
	else '' SHR
		a = dst1
		av = dvreg
		b = dst2
		bv = dvreg->vaux
	end if

	if( svreg->typ = IR_VREGTYPE_IMM ) then
		if( svreg->value.int >= 64 ) then
			'' zero both result halves
			if( bv->typ = IR_VREGTYPE_REG ) then
				outp "xorl " + b + ", " + b
			else
				outp "movl $0, " + b
			end if

			if( av->typ = IR_VREGTYPE_REG ) then
				outp "xorl " + a + ", " + a
			else
				outp "movl $0, " + a
			end if
		elseif( svreg->value.int >= 32 ) then
			if( (bv->typ = IR_VREGTYPE_REG) or (av->typ = IR_VREGTYPE_REG) ) then
				'' a or b is a reg
				outp "movl " + b + ", " + a
			else
				'' neither is a reg; get a temp
				tmpreg = hFindFreeReg( FB_DATACLASS_INTEGER )
				if( tmpreg = INVALID ) then
					tmpreg = EMIT_REG_EAX
					hPUSHAtt( "%eax" )
					preserveeax = TRUE
				end if
				tmpregname = *hGetRegNameAtt( FB_DATATYPE_INTEGER, tmpreg )
				outp "movl " + b + ", " + tmpregname
				outp "movl " + tmpregname + ", " + a
			end if

			if( (op = AST_OP_SHR) and symbIsSigned( dvreg->dtype ) ) then
				outp "sarl $31, " + b
			elseif( bv->typ = IR_VREGTYPE_REG ) then
				outp "xorl " + b + ", " + b
			else
				outp "movl $0, " + b
			end if

			if( svreg->value.int > 32 ) then
				src = str( svreg->value.int - 32 )
				outp mnemonic32 + src + ", " + a
			end if

			if( preserveeax ) then
				hPOPAtt( "%eax" )
			end if

		else '' src < 32
			if( bv->typ = IR_VREGTYPE_REG ) then
				outp mnemonic64 + src + ", " + b + ", " + a
				outp mnemonic32 + src + ", " + b
			elseif( av->typ = IR_VREGTYPE_REG ) then
				outp "xchgl " + a + ", " + b
				outp mnemonic64 + src + ", " + a + ", " + b
				outp mnemonic32 + src + ", " + a
				outp "xchgl " + a + ", " + b
			else
				tmpreg = hFindFreeReg( FB_DATACLASS_INTEGER )
				if( tmpreg = INVALID ) then
					tmpreg = EMIT_REG_EAX
					hPUSHAtt( "%eax" )
					preserveeax = TRUE
				end if
				tmpregname = *hGetRegNameAtt( FB_DATATYPE_INTEGER, tmpreg )
				outp "movl " + b + ", " + tmpregname
				outp mnemonic64 + src + ", " + tmpregname + ", " + a
				outp mnemonic32 + src + ", " + tmpregname
				outp "movl " + tmpregname + ", " + b

				if( preserveeax ) then
					hPOPAtt( "%eax" )
				end if
			end if

		end if

	else
		'' if src is not an imm, use cl and check for the x86 glitches

		dim as integer iseaxfree, isedxfree, isecxfree
		dim as integer eaxindest, edxindest, ecxindest
		dim as integer ofs

		label = *hMakeTmpStr( )

		hPUSHAtt( dst2 )
		hPUSHAtt( dst1 )
		ofs = 0

		iseaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
		isedxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )
		isecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )

		eaxindest = hIsRegInVreg( dvreg, EMIT_REG_EAX )
 		edxindest = hIsRegInVreg( dvreg, EMIT_REG_EDX )
 		ecxindest = hIsRegInVreg( dvreg, EMIT_REG_ECX )

		if( (svreg->typ <> IR_VREGTYPE_REG) or (svreg->reg <> EMIT_REG_ECX) ) then
			'' handle src < dword
			if( symbGetDataSize( svreg->dtype ) <> FB_INTEGERSIZE ) then
 				'' if it's not a reg, the right size was already set at the hPrepOperandAtt() above
 				if( svreg->typ = IR_VREGTYPE_REG ) then
 					src = *hGetRegNameAtt( FB_DATATYPE_INTEGER, svreg->reg )
 				end if
 			end if

 			if( isecxfree = FALSE ) then
 				if( ecxindest and dvreg->typ = IR_VREGTYPE_REG ) then
 					hMOVAtt( "%ecx", src )
 					isecxfree = TRUE
 				else
 					hPUSHAtt( src )
 					outp "xchgl (%esp), %ecx"
 					ofs += 4
 				end if
 			else
 				hMOVAtt( "%ecx", src )
 			end if
 		else
 			isecxfree = TRUE
 		end if

		'' load dst1 to eax
 		if( eaxindest ) then
 			if( dvreg->typ <> IR_VREGTYPE_REG ) then
 				outp "xchgl " + str( ofs+0 ) + "(%esp), %eax"
 			else
 				outp "movl " + str( ofs+0 ) + "(%esp), %eax"
 			end if
 		else
 			if( iseaxfree = FALSE ) then
 				outp "xchgl %eax, " + str( ofs+0 ) + "(%esp)"
 			else
 				outp "movl " + str( ofs+0 ) + "(%esp), %eax"
 			end if
 		end if

 		'' load dst2 to edx
 		if( edxindest ) then
 			if( dvreg->typ <> IR_VREGTYPE_REG ) then
 				outp "xchgl %edx, " + str( ofs+4 ) + "(%esp)"
 			else
 				outp "movl " + str( ofs+4 ) + "(%esp), %edx"
 			end if
 		else
 			if( isedxfree = FALSE ) then
 				outp "xchgl " + str( ofs+4 ) + "(%esp), %edx"
 			else
 				outp "movl " + str( ofs+4 ) + "(%esp), %edx"
 			end if
 		end if

		if( op = AST_OP_SHL ) then
 			outp "shldl %cl, %eax, %edx"
 			outp mnemonic32 + " %cl, %eax"
 		else
 			outp "shrdl %cl, %edx, %eax"
 			outp mnemonic32 + " %cl, %edx"
 		end if

 		outp "test $32, %cl"
 		hBRANCH( "jz", label )

 		if( op = AST_OP_SHL ) then
 			outp "movl %eax, %edx"
 			outp "xorl %eax, %eax"
 		else
 			outp "movl %edx, %eax"
 			if( symbIsSigned( dvreg->dtype ) ) then
 				outp "sarl $31, %edx"
 			else
 				outp "xorl %edx, %edx"
 			end if
 		end if

 		hLABEL( label )

 		if( isecxfree = FALSE ) then
 			hPOPAtt "%ecx"
 		end if

		'' save dst2
 		if( edxindest ) then
 			if( dvreg->typ <> IR_VREGTYPE_REG ) then
 				outp "xchgl 4(%esp), %edx"
 			else
 				outp "movl %edx, 4(%esp)"
 			end if
 		else
 			if( isedxfree = FALSE ) then
 				outp "xchgl %edx, 4(%esp)"
 			else
 				outp "movl %edx, 4(%esp)"
 			end if
 		end if

 		'' save dst1
 		if( eaxindest ) then
 			if( dvreg->typ <> IR_VREGTYPE_REG ) then
 				outp "xchgl %eax, 0(%esp)"
 			else
 				outp "movl %eax, 0(%esp)"
 			end if
 		else
 			if( iseaxfree = FALSE ) then
 				outp "xchgl %eax, 0(%esp)"
 			else
 				outp "movl %eax, 0(%esp)"
 			end if
 		end if

		hPOPAtt( dst1 )
		hPOPAtt( dst2 )
	end if

end sub

'':::::
private sub hSHIFTI _
	( _
		byval op as integer, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim eaxpreserved as integer, ecxpreserved as integer
    dim eaxfree as integer, ecxfree as integer
    dim reg as integer
    dim ecxindest as integer
    dim as string ostr, dst, src, tmp, mnemonic

	''
	if( symbIsSigned( dvreg->dtype ) ) then
		if( op = AST_OP_SHL ) then
			mnemonic = "sal"
		else
			mnemonic = "sar"
		end if
	else
		if( op = AST_OP_SHL ) then
			mnemonic = "shl"
		else
			mnemonic = "shr"
		end if
	end if
	
	mnemonic += dtsuffixTB(dvreg->dtype).mname

    ''
	hPrepOperandAtt( dvreg, dst )

	ecxindest = FALSE
	eaxpreserved = FALSE
	ecxpreserved = FALSE

	if( svreg->typ = IR_VREGTYPE_IMM ) then
		hPrepOperandAtt( svreg, src )
		tmp = dst

	else
		eaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
		ecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )

		if( svreg->typ = IR_VREGTYPE_REG ) then
			reg = svreg->reg
		else
			reg = INVALID
		end if

        ecxindest = hIsRegInVreg( dvreg, EMIT_REG_ECX )

		'' ecx in destine?
		if( ecxindest ) then
			'' preserve
			hPUSHAtt "%ecx"
			'' not a reg?
			if( dvreg->typ <> IR_VREGTYPE_REG ) then
				hPrepOperandAtt( dvreg, ostr, FB_DATATYPE_INTEGER )
				hPUSHAtt ostr
			end if

		'' ecx not free?
		elseif( (reg <> EMIT_REG_ECX) and (ecxfree = FALSE) ) then
			ecxpreserved = TRUE
			hPUSHAtt "%ecx"
		end if

		'' source not a reg?
		if( svreg->typ <> IR_VREGTYPE_REG ) then
			hPrepOperandAtt( svreg, ostr, FB_DATATYPE_BYTE )
			hMOVAtt "%cl", ostr, FB_DATATYPE_BYTE
		else
			'' source not ecx?
			if( reg <> EMIT_REG_ECX ) then
				hMOVAtt "%ecx", rnameattTB(dtypeTB(FB_DATATYPE_INTEGER).rnametb, reg)
			end if
		end if

		'' load ecx to a tmp?
		if( ecxindest ) then
			'' tmp not free?
			if( eaxfree = FALSE ) then
				eaxpreserved = TRUE
				outp "xchgl %eax, (%esp)"	'' eax= dst; push eax
			else
				hPOPAtt "%eax"				'' eax= dst; pop tos
			end if

			tmp = rnameattTB(dtypeTB(dvreg->dtype).rnametb, EMIT_REG_EAX )

		else
			tmp = dst
		end if

		src = "%cl"

	end if

	ostr = mnemonic + " " + src + COMMA + tmp
	outp ostr

	if( ecxindest ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hPOPAtt "%ecx"
			if( eaxpreserved ) then
				outp "xchgl %ecx, (%esp)"	'' ecx= tos; tos= eax
			end if
		end if
		hMOVAtt dst, rnameattTB(dtypeTB(dvreg->dtype).rnametb, EMIT_REG_EAX), dvreg->dtype
	end if

	if( eaxpreserved ) then
		hPOPAtt "%eax"
	end if

	if( ecxpreserved ) then
		hPOPAtt "%ecx"
	end if

end sub

'':::::
private sub _emitAttSHLL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hSHIFTL( AST_OP_SHL, dvreg, svreg )

end sub

'':::::
private sub _emitAttSHLI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hSHIFTI( AST_OP_SHL, dvreg, svreg )

end sub

'':::::
private sub _emitAttSHRL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hSHIFTL( AST_OP_SHR, dvreg, svreg )

end sub

'':::::
private sub _emitAttSHRI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hSHIFTI( AST_OP_SHR, dvreg, svreg )

end sub

'':::::
private sub _emitAttANDL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "andl " + src1 + COMMA + dst1
	outp ostr

	ostr = "andl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttANDI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ostr = "and" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
	outp ostr

end sub

'':::::
private sub _emitAttORL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "orl " + src1 + COMMA + dst1
	outp ostr

	ostr = "orl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttORI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ostr = "or" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
	outp ostr

end sub

'':::::
private sub _emitAttXORL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "xorl " + src1 + COMMA + dst1
	outp ostr

	ostr = "xorl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttXORI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ostr = "xor" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
	outp ostr

end sub

'':::::
private sub _emitAttEQVL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "xorl " + src1 + COMMA + dst1
	outp ostr

	ostr = "xorl " + src2 + COMMA + dst2
	outp ostr

	ostr = "notl " + dst1
	outp ostr

	ostr = "notl " + dst2
	outp ostr

end sub

'':::::
private sub _emitAttEQVI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ostr = "xor" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
	outp ostr

	ostr = "not" + dtsuffixTB(dvreg->dtype).mname + " " + dst
	outp ostr

end sub

'':::::
private sub _emitAttIMPL _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string, src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "notl " + dst1
	outp ostr

	ostr = "notl " + dst2
	outp ostr

	ostr = "orl " + src1 + COMMA + dst1
	outp ostr

	ostr = "orl " + src2 + COMMA + dst2
	outp ostr

end sub

'':::::
private sub _emitAttIMPI _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim dst as string, src as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ostr = "not" + dtsuffixTB(dvreg->dtype).mname + " " + dst
	outp ostr

	ostr = "or" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
	outp ostr

end sub


'':::::
private sub _emitAttATN2 _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim src as string
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	if( svreg->typ <> IR_VREGTYPE_REG ) then
		ostr = "fld" + dtsuffixTB(svreg->dtype).mfl + " " + src
		outp ostr
	else
		outp "fxch"
	end if
	outp "fpatan"

end sub

'':::::
private sub _emitAttPOW _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim src as string
	dim ostr as string

	hPrepOperandAtt( svreg, src )

	if( svreg->typ <> IR_VREGTYPE_REG ) then
		ostr = "fld" + dtsuffixTB(svreg->dtype).mfl + " " + src
		outp ostr
		outp "fxch"
	end if

	outp "fabs"
	outp "fyl2x"
	outp "fld %st(0)"
	outp "frndint"
	outp "fsub %st(1), %st(0)" ''TODO: is this right??
	outp "fxch"
	outp "f2xm1"
	outp "fld1"
	outp "faddp"
	outp "fscale"
	outp "fstp %st(1)"

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' relational
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub hCMPL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval mnemonic as zstring ptr, _
		byval rev_mnemonic as zstring ptr, _
		byval usg_mnemonic as zstring ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval isinverse as integer = FALSE _
	) static

    dim as string dst1, dst2, src1, src2, rname, ostr, lname, falselabel

	hPrepOperandAtt64( dvreg, dst1, dst2 )
	hPrepOperandAtt64( svreg, src1, src2 )

	if( label = NULL ) then
		lname = *hMakeTmpStr( )
	else
		lname = *symbGetMangledName( label )
	end if

	'' check high
	ostr = "cmpl " + src2 + COMMA + dst2
	outp ostr

	falselabel = *hMakeTmpStr( )

	'' set the boolean result?
	if( rvreg <> NULL ) then
		hPrepOperandAtt( rvreg, rname )
		hMOVAtt( rname, "$-1" )
	end if

	ostr = "j" + *mnemonic
	if( isinverse = FALSE ) then
		hBRANCH( ostr, lname )
	else
		hBRANCH( ostr, falselabel )
	end if

	if( len( *rev_mnemonic ) > 0 ) then
		ostr = "j" + *rev_mnemonic
		hBRANCH( ostr, falselabel )
	end if

	'' check low
	ostr = "cmpl " + src1 + COMMA + dst1
	outp ostr

	ostr = "j" + *usg_mnemonic
	hBRANCH( ostr, lname )

	hLabel( falselabel )

	if( rvreg <> NULL ) then
		ostr = "xorl " + rname + COMMA + rname
		outp ostr

		hLabel( lname )
	end if

end sub

'':::::
private sub hCMPI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval mnemonic as zstring ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

    dim as string rname, rname8, dst, src, ostr, lname
    dim as integer isedxfree, dotest

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	if( label = NULL ) then
		lname = *hMakeTmpStr( )
	else
		lname = *symbGetMangledName( label )
	end if

	'' optimize "cmp" to "test"
	dotest = FALSE
	if( (svreg->typ = IR_VREGTYPE_IMM) and (dvreg->typ = IR_VREGTYPE_REG) ) then
		if( svreg->value.int = 0 ) then
			dotest = TRUE
		end if
	end if

	if( dotest ) then
		ostr = "test" + dtsuffixTB(dvreg->dtype).mname + " " + dst + COMMA + dst
		outp ostr
	else
		ostr = "cmp" + dtsuffixTB(dvreg->dtype).mname + " " + src + COMMA + dst
		outp ostr
	end if

	'' no result to be set? just branch
	if( rvreg = NULL ) then
		ostr = "j" + *mnemonic
		hBRANCH( ostr, lname )
		exit sub
	end if

	hPrepOperandAtt( rvreg, rname )

	'' can it be optimized?
	if( (env.clopt.cputype >= FB_CPUTYPE_486) and (rvreg->typ = IR_VREGTYPE_REG) ) then

		rname8 = *hGetRegNameAtt( FB_DATATYPE_BYTE, rvreg->reg )

		'' handle EDI and ESI
		if( (rvreg->reg = EMIT_REG_ESI) or (rvreg->reg = EMIT_REG_EDI) ) then

			isedxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )
			if( isedxfree = FALSE ) then
				ostr = "xchgl " + rname + ", %edx"
				outp ostr
			end if

			ostr = "set" + *mnemonic + " %dl"
			outp ostr

			if( isedxfree = FALSE ) then
				ostr = "xchgl " + rname + ", %edx"
				outp ostr
			else
				hMOVAtt rname, "%edx"
			end if

		else
			ostr = "set" + *mnemonic + " " + rname8
			outp ostr
		end if

		'' convert 1 to -1 (TRUE in QB/FB)
		ostr = "shrl $1, " + rname
		outp ostr

		ostr = "sbbl " + rname + COMMA + rname
		outp ostr

	'' old (and slow) boolean set
	else

		ostr = "movl $-1, " + rname
		outp ostr

		ostr = "j" + *mnemonic
		hBRANCH( ostr, lname )

		ostr = "xorl " + rname + COMMA + rname
		outp ostr

		hLabel( lname )
	end if

end sub


'':::::
private sub hCMPF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval mnemonic as zstring ptr, _
		byval mask as zstring ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim as string rname, rname8, dst, src, ostr, lname
    dim as integer iseaxfree, isedxfree

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	if( label = NULL ) then
		lname = *hMakeTmpStr( )
	else
		lname = *symbGetMangledName( label )
	end if

	'' do comp
	if( svreg->typ = IR_VREGTYPE_REG ) then
		outp "fcompp"
	else
		'' can it be optimized to ftst?
		if( symbGetDataClass( svreg->dtype ) = FB_DATACLASS_FPOINT ) then
			ostr = "fcomp" + dtsuffixTB(svreg->dtype).mfl + " " + src
			outp ostr
		else
			ostr = "ficomp" + dtsuffixTB(svreg->dtype).mname + " " + src
			outp ostr
		end if
	end if

    iseaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
    if( rvreg <> NULL ) then
    	iseaxfree = (rvreg->reg = EMIT_REG_EAX)
	end if

    if( iseaxfree = FALSE ) then
    	hPUSHAtt( "%eax" )
    end if

    '' load fpu flags
    outp "fnstsw %ax"
	if( len( *mask ) > 0 ) then
		ostr = "test " + *mask + ", %ah"
		outp ostr
	else
		outp "sahf"
	end if

	if( iseaxfree = FALSE ) then
		hPOPAtt( "%eax" )
	end if

    '' no result to be set? just branch
    if( rvreg = NULL ) then
    	ostr = "j" + *mnemonic
    	hBRANCH( ostr, lname )
    	exit sub
    end if

    hPrepOperandAtt( rvreg, rname )

	'' can it be optimized?
	if( env.clopt.cputype >= FB_CPUTYPE_486 ) then
		rname8 = *hGetRegNameAtt( FB_DATATYPE_BYTE, rvreg->reg )

		'' handle EDI and ESI
		if( (rvreg->reg = EMIT_REG_ESI) or (rvreg->reg = EMIT_REG_EDI) ) then

			isedxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDX )
			if( isedxfree = FALSE ) then
				ostr = "xchgl " + rname + ", %edx"
				outp ostr
			end if

			ostr = "set" + *mnemonic + (TABCHAR + "%dl")
			outp ostr

			if( isedxfree = FALSE ) then
				ostr = "xchgl %edx, " + rname
				outp ostr
			else
				hMOVAtt rname, "%edx"
			end if
		else
			ostr = "set" + *mnemonic + " " + rname8
			outp ostr
		end if

		'' convert 1 to -1 (TRUE in QB/FB)
		ostr = "shrl $1, " + rname
		outp ostr

		ostr = "sbbl " + rname + COMMA + rname
		outp ostr

	'' old (and slow) boolean set
	else
    	ostr = "movl $-1, " + rname
    	outp ostr

    	ostr = "j" + *mnemonic
    	hBRANCH( ostr, lname )

		ostr = "xorl " + rname + COMMA + rname
		outp ostr

		hLabel( lname )
	end if

end sub

'':::::
private sub _emitAttCGTL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string, rjmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "g"
		rjmp = "l"
	else
		jmp = "a"
		rjmp = "b"
	end if

	hCMPL( rvreg, label, jmp, rjmp, "a", dvreg, svreg )

end sub

'':::::
private sub _emitAttCGTI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "g"
	else
		jmp = "a"
	end if

	hCMPI( rvreg, label, jmp, dvreg, svreg )

end sub



'':::::
private sub _emitAttCGTF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPF( rvreg, label, "z", "$0b01000001", dvreg, svreg )

end sub

'':::::
private sub _emitAttCLTL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string, rjmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "l"
		rjmp = "g"
	else
		jmp = "b"
		rjmp = "a"
	end if

	hCMPL( rvreg, label, jmp, rjmp, "b", dvreg, svreg )

end sub

'':::::
private sub _emitAttCLTI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "l"
	else
		jmp = "b"
	end if

	hCMPI( rvreg, label, jmp, dvreg, svreg )

end sub


'':::::
private sub _emitAttCLTF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPF( rvreg, label, "nz", "$0b00000001", dvreg, svreg )

end sub

'':::::
private sub _emitAttCEQL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPL( rvreg, label, "ne", "", "e", dvreg, svreg, TRUE )

end sub

'':::::
private sub _emitAttCEQI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPI( rvreg, label, "e", dvreg, svreg )

end sub


'':::::
private sub _emitAttCEQF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPF( rvreg, label, "nz", "$0b01000000", dvreg, svreg )

end sub

'':::::
private sub _emitAttCNEL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPL( rvreg, label, "ne", "", "ne", dvreg, svreg )

end sub

'':::::
private sub _emitAttCNEI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPI( rvreg, label, "ne", dvreg, svreg )

end sub


'':::::
private sub _emitAttCNEF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPF( rvreg, label, "z", "$0b01000000", dvreg, svreg )

end sub

'':::::
private sub _emitAttCLEL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string, rjmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "l"
		rjmp = "g"
	else
		jmp = "b"
		rjmp = "a"
	end if

	hCMPL( rvreg, label, jmp, rjmp, "be", dvreg, svreg )

end sub

'':::::
private sub _emitAttCLEI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "le"
	else
		jmp = "be"
	end if

	hCMPI( rvreg, label, jmp, dvreg, svreg )

end sub


'':::::
private sub _emitAttCLEF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPF( rvreg, label, "nz", "$0b01000001", dvreg, svreg )

end sub


'':::::
private sub _emitAttCGEL _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string, rjmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "g"
		rjmp = "l"
	else
		jmp = "a"
		rjmp = "b"
	end if

	hCMPL( rvreg, label, jmp, rjmp, "ae", dvreg, svreg )

end sub

'':::::
private sub _emitAttCGEI _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim jmp as string

	if( symbIsSigned( dvreg->dtype ) ) then
		jmp = "ge"
	else
		jmp = "ae"
	end if

	hCMPI( rvreg, label, jmp, dvreg, svreg )

end sub


'':::::
private sub _emitAttCGEF _
	( _
		byval rvreg as IRVREG ptr, _
		byval label as FBSYMBOL ptr, _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	hCMPF( rvreg, label, "ae", "", dvreg, svreg )

end sub


''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' unary ops
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttNEGL _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	ostr = "negl " + dst1
	outp ostr

	ostr = "adcl $0, " + dst2
	outp ostr

	ostr = "negl " + dst2
	outp ostr

end sub

'':::::
private sub _emitAttNEGI _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )

	ostr = "neg" + dtsuffixTB(dvreg->dtype).mname + " " + dst
	outp ostr

end sub


'':::::
private sub _emitAttNEGF _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fchs"

end sub

'':::::
private sub _emitAttNOTL _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	ostr = "notl " + dst1
	outp ostr

	ostr = "notl " + dst2
	outp ostr

end sub

'':::::
private sub _emitAttNOTI _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst as string
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )

	ostr = "not" + dtsuffixTB(dvreg->dtype).mname + " " + dst
	outp ostr

end sub

'':::::
private sub _emitAttABSL _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string
    dim reg as integer, isfree as integer, rname as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	reg = hFindRegNotInVreg( dvreg )
	rname = *hGetRegNameAtt( FB_DATATYPE_INTEGER, reg )

	isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

	if( isfree = FALSE ) then
		hPUSHAtt( rname )
	end if

	hMOVAtt( rname, dst2 )

	ostr = "sarl $31, " + rname
	outp ostr

	ostr = "xorl " + rname + COMMA + dst1
	outp ostr

	ostr = "xorl " + rname + COMMA + dst2
	outp ostr

	ostr = "subl " + rname + COMMA + dst1
	outp ostr

	ostr = "sbbl " + rname + COMMA + dst2
	outp ostr

	if( isfree = FALSE ) then
		hPOPAtt( rname )
	end if

end sub

'':::::
private sub _emitAttABSI _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst as string
    dim reg as integer, isfree as integer, rname as string, bits as integer
    dim ostr as string

	hPrepOperandAtt( dvreg, dst )

	reg = hFindRegNotInVreg( dvreg )
	rname = *hGetRegNameAtt( dvreg->dtype, reg )

	isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )

	if( isfree = FALSE ) then
		hPUSHAtt( rname )
	end if

	bits = (dtypeTB(dvreg->dtype).size * 8)-1

	hMOVAtt( rname, dst )

	ostr = "sar" + dtsuffixTB(dvreg->dtype).mname
	ostr += " $" + str( bits ) + COMMA + rname
	outp ostr

	ostr = "xor" + dtsuffixTB(dvreg->dtype).mname + " " + rname + COMMA + dst
	outp ostr

	ostr = "sub" + dtsuffixTB(dvreg->dtype).mname + " " + rname + COMMA + dst
	outp ostr

	if( isfree = FALSE ) then
		hPOPAtt( rname )
	end if

end sub


'':::::
private sub _emitAttABSF _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fabs"

end sub

'':::::
private sub _emitAttSGNL _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim dst1 as string, dst2 as string
    dim ostr as string
    dim label1 as string, label2 as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	label1 = *hMakeTmpStr( )
	label2 = *hMakeTmpStr( )

	ostr = "cmpl $0, " + dst2
	outp ostr
	hBRANCH( "jne", label1 )

	ostr = "cmpl $0, " + dst1
	outp ostr
	hBRANCH( "je", label2 )

	hLABEL( label1 )
	hMOVAtt( dst1, "$1" )
	hMOVAtt( dst2, "$0" )
	hBRANCH( "jg", label2 )
	hMOVAtt( dst1, "$-1" )
	hMOVAtt( dst2, "$-1" )

	hLABEL( label2 )

end sub

'':::::
private sub _emitAttSGNI _
	( _
		byval dvreg as IRVREG ptr _
	) static

    dim as string dst, label, ostr

	hPrepOperandAtt( dvreg, dst )

	label = *hMakeTmpStr( )

	ostr = "cmp" + dtsuffixTB(dvreg->dtype).mname + " $0, " + dst
	outp ostr

	hBRANCH( "je", label )
	hMOVAtt( dst, "$1" )
	hBRANCH( "jg", label )
	hMOVAtt( dst, "$-1" )

	hLABEL( label )

end sub


'':::::
private sub _emitAttSGNF _
	( _
		byval dvreg as IRVREG ptr _
	) static

	dim as string dst, label, ostr
	dim as integer iseaxfree

	hPrepOperandAtt( dvreg, dst )

	label = *hMakeTmpStr( )

    iseaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )

    if( iseaxfree = FALSE ) then
    	hPUSHAtt( "%eax" )
    end if

	outp "ftst"
	outp "fnstsw %ax"
	outp "sahf"

	if( iseaxfree = FALSE ) then
		hPOPAtt( "%eax" )
	end if

	'' if dst = 0
	hBRANCH( "jz", label )
	'' elseif dst > 0
	outp "fstp %st(0)"
	outp "fld1"
	hBRANCH( "ja", label )
	'' else
	outp "fchs"

	hLABEL( label )

end sub

'':::::
private sub _emitAttSIN _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fsin"

end sub

'':::::
private sub _emitAttASIN _
	( _
		byval dvreg as IRVREG ptr _
	) static

	'' asin( x ) = atn( sqr( (x*x) / (1-x*x) ) )
	outp "fld %st(0)"
    outp "fmul %st(0), %st(0)"
    outp "fld1"
	outp "fsubrp"
	outp "fsqrt"
	outp "fpatan"

end sub

'':::::
private sub _emitAttCOS _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fcos"

end sub

'':::::
private sub _emitAttACOS _
	( _
		byval dvreg as IRVREG ptr _
	) static

	'' acos( x ) = atn( sqr( (1-x*x) / (x*x) ) )
	outp "fld %st(0)"
    outp "fmul %st(0), %st(0)"
    outp "fld1"
	outp "fsubrp"
	outp "fsqrt"
	outp "fxch"
	outp "fpatan"

end sub

'':::::
private sub _emitAttTAN _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fptan"
	outp "fstp %st(0)"

end sub

'':::::
private sub _emitAttATAN _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fld1"
	outp "fpatan"

end sub

'':::::
private sub _emitAttSQRT _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fsqrt"

end sub

'':::::
private sub _emitAttLOG _
	( _
		byval dvreg as IRVREG ptr _
	) static

	'' log( x ) = log2( x ) / log2( e ).

    outp "fldln2"
    outp "fxch"
    outp "fyl2x"

end sub

'':::::
private sub _emitAttEXP _
	( _
		byval dvreg as IRVREG ptr _
	) static

	outp "fldl2e"
	outp "fmulp %st(1), %st(0)" ''TODO: is this right att syntax?
	outp "fld %st"
    outp "frndint"
	outp "fsub %st(1), %st(0)"
	outp "fxch"
	outp "f2xm1"
	'' can't use fld1 because max 2 fp regs can be used
	hPUSHAtt( "$0x3f800000" )
	outp "faddl (%esp)"
	outp "addl $4, %esp"
	outp "fscale"
	outp "fstp %st(1)"
end sub



''::::
#macro hFpuChangeRC( cw_reg, mode )
	scope
		static as string ostr
		outp "sub $4, %esp"
		outp "fnstcw (%esp)"
		hMOVAtt cw_reg, "(%esp)"
		if( mode <> "11" ) then
			ostr = "andl $0b1111001111111111, " + cw_reg
			outp ostr
		end if
		ostr = ("orl $0b0000" + mode + "0000000000, ") + cw_reg
		outp ostr
		hPUSHAtt cw_reg
		outp "fldcw (%esp)"
		outp "addl $4, %esp"
	end scope
#endmacro

''::::
#macro hFpuRoundRestore( )
	outp "fldcw (%esp)"
	outp "addl $4, %esp"
#endmacro

'':::::
private sub _emitAttFLOOR _
	( _
		byval dvreg as IRVREG ptr _
	) static

	dim as integer cw_reg, isfree
	dim as string cw_regname

	cw_reg = hFindFreeReg( FB_DATACLASS_INTEGER )
	cw_regname = *hGetRegNameAtt( FB_DATATYPE_INTEGER, cw_reg )

	isfree = hIsRegFree( FB_DATACLASS_INTEGER, cw_reg )

	if( isfree = FALSE ) then
		hPUSHAtt( cw_regname )
	end if

	'' round down toward -infinity
	hFpuChangeRC( cw_regname, "01" )

	outp "frndint"

	hFpuRoundRestore( )

	if( isfree = FALSE ) then
		hPOPAtt( cw_regname )
	end if

end sub

'':::::
private sub _emitAttFIX _
	( _
		byval dvreg as IRVREG ptr _
	) static

	dim as integer cw_reg, isfree
	dim as string cw_regname

	'' dst = floor( abs( dst ) ) * sng( dst )

	cw_reg = hFindFreeReg( FB_DATACLASS_INTEGER )
	cw_regname = *hGetRegNameAtt( FB_DATATYPE_INTEGER, cw_reg )

	isfree = hIsRegFree( FB_DATACLASS_INTEGER, cw_reg )

	if( isfree = FALSE ) then
		hPUSHAtt( cw_regname )
	end if

	'' chop truncating toward 0
	hFpuChangeRC( cw_regname, "11" )

	outp "frndint"

	hFpuRoundRestore( )

	if( isfree = FALSE ) then
		hPOPAtt( cw_regname )
	end if

end sub

'':::::
private sub _emitAttFRAC _
	( _
		byval dvreg as IRVREG ptr _
	) static

	dim as integer cw_reg, isfree
	dim as string cw_regname

	'' dst = dst - floor( dst )

	cw_reg = hFindFreeReg( FB_DATACLASS_INTEGER )
	cw_regname = *hGetRegNameAtt( FB_DATATYPE_INTEGER, cw_reg )

	isfree = hIsRegFree( FB_DATACLASS_INTEGER, cw_reg )

	if( isfree = FALSE ) then
		hPUSHAtt( cw_regname )
	end if

	'' chop truncating toward 0
	hFpuChangeRC( cw_regname, "11" )

	outp "fld %st(0)"
	outp "frndint"
	outp "fsubp"

	hFpuRoundRestore( )

	if( isfree = FALSE ) then
		hPOPAtt( cw_regname )
	end if

end sub


'':::::
private sub _emitAttXchgTOS _
	( _
		byval svreg as IRVREG ptr _
	) static

    dim as string src
    dim as string ostr

	hPrepOperandAtt( svreg, src )

	ostr = "fxch " + src
	outp( ostr )

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' stack
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttPUSHL _
	( _
		byval svreg as IRVREG ptr, _
		byval unused as integer _
	) static

    dim src1 as string, src2 as string
    dim ostr as string

	hPrepOperandAtt64( svreg, src1, src2 )

	ostr = "pushl " + src2
	outp ostr

	ostr = "pushl " + src1
	outp ostr

end sub

'':::::
private sub _emitAttPUSHI _
	( _
		byval svreg as IRVREG ptr, _
		byval unused as integer _
	) static

    dim src as string, sdsize as integer
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	sdsize = symbGetDataSize( svreg->dtype )

	if( svreg->typ = IR_VREGTYPE_REG ) then
		if( sdsize < FB_INTEGERSIZE ) then
			src = *hGetRegNameAtt( FB_DATATYPE_INTEGER, svreg->reg )
		end if
	else
		if( sdsize < FB_INTEGERSIZE ) then
			'' !!!FIXME!!! assuming it's okay to push over the var if's not dword aligned
			hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER )
		end if
	end if

	ostr = "pushl " + src
	outp ostr

end sub


'':::::
private sub _emitAttPUSHF _
	( _
		byval svreg as IRVREG ptr, _
		byval unused as integer _
	) static

    dim src as string, sdsize as integer
    dim ostr as string

	hPrepOperandAtt( svreg, src )

	sdsize = symbGetDataSize( svreg->dtype )

	if( svreg->typ <> IR_VREGTYPE_REG ) then
		if( svreg->dtype = FB_DATATYPE_SINGLE ) then
			ostr = "pushl " + src
			outp ostr
		else
			hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER, 4 )
			ostr = "pushl " + src
			outp ostr

    		hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER, 0 )
			ostr = "pushl " + src
			outp ostr
		end if
	else
		ostr = "subl $" + str( sdsize ) + ", %esp"
		outp ostr

		ostr = "fstp" + dtsuffixTB(svreg->dtype).mfl
		ostr += " (%esp)"
		outp ostr
	end if

end sub

'':::::
private sub _emitAttPUSHUDT _
	( _
		byval svreg as IRVREG ptr, _
		byval sdsize as integer _
	) static

    dim as integer ofs
    dim as string ostr, src

	'' !!!FIXME!!! assuming it's okay to push over the UDT if's not dword aligned
	if( sdsize < FB_INTEGERSIZE ) then
		sdsize = FB_INTEGERSIZE
	end if

	ofs = sdsize - FB_INTEGERSIZE
	do while( ofs >= 0 )
		hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER, ofs )
		ostr = "pushl " + src
		outp( ostr )
		ofs -= FB_INTEGERSIZE
	loop

end sub

'':::::
private sub _emitAttPOPL _
	( _
		byval dvreg as IRVREG ptr, _
		byval unused as integer _
	) static

    dim dst1 as string, dst2 as string
    dim ostr as string

	hPrepOperandAtt64( dvreg, dst1, dst2 )

	ostr = "popl " + dst1
	outp ostr

	ostr = "popl " + dst2
	outp ostr

end sub

'':::::
private sub _emitAttPOPI _
	( _
		byval dvreg as IRVREG ptr, _
		byval unused as integer _
	) static

    dim as string dst, ostr
    dim as integer dsize

	hPrepOperandAtt( dvreg, dst )

	dsize = symbGetDataSize( dvreg->dtype )

	if( dvreg->typ = IR_VREGTYPE_IMM ) then
		'' gosub quirk: return-to-label needs to pop return address from the stack
		'' see ast-gosub.bas::astGosubAddReturn() - (jeffm)

		if( dvreg->value.int = 4 ) then
			if( hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX ) ) then
				hPOPAtt "%eax"
			else
				outp "addl $4, %esp"
			end if
		else
			ostr = "addl $" + str( dvreg->value.int ) + ", %esp"
			outp ostr
		end if

	elseif( dsize = FB_INTEGERSIZE ) then
		ostr = "popl " + dst
		outp ostr

	else
		if( dvreg->typ = IR_VREGTYPE_REG ) then
			dst = *hGetRegNameAtt( FB_DATATYPE_INTEGER, dvreg->reg )
			ostr = "popl " + dst
			outp ostr
		else
			outp "xchg %eax, (%esp)"

			if( dsize = 1 ) then
				hMOVAtt dst, "%al", FB_DATATYPE_BYTE
			else
				hMOVAtt dst, "%ax", FB_DATATYPE_SHORT
			end if

			if( hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX ) = FALSE ) then
				hPOPAtt "%eax"
			else
				outp "addl $4, %esp"
			end if
		end if

	end if

end sub


'':::::
private sub _emitAttPOPF _
	( _
		byval dvreg as IRVREG ptr, _
		byval unused as integer _
	) static

    dim as string dst, ostr
    dim as integer dsize

	hPrepOperandAtt( dvreg, dst )

	dsize = symbGetDataSize( dvreg->dtype )

	if( dvreg->typ <> IR_VREGTYPE_REG ) then
		if( dvreg->dtype = FB_DATATYPE_SINGLE ) then
			ostr = "popl " + dst
			outp ostr
		else
			hPrepOperandAtt( dvreg, dst, FB_DATATYPE_INTEGER, 0 )
			ostr = "popl " + dst
			outp ostr

			hPrepOperandAtt( dvreg, dst, FB_DATATYPE_INTEGER, 4 )
			ostr = "popl " + dst
			outp ostr
		end if
	else
		ostr = "fld" + dtsuffixTB(dvreg->dtype).mname
		ostr += " (%esp)"
		outp ostr

		ostr = "addl $" + str( dsize ) + ", %esp"
		outp ostr
	end if

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' addressing
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttADDROF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim as string dst, src
	dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src, , , , FALSE )

	ostr = "leal " + src + ", " + dst
	outp ostr

end sub

'':::::
private sub _emitAttDEREF _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr _
	) static

	dim as string dst, src
	dim as string ostr

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src, FB_DATATYPE_UINT )

	ostr = "movl " + src + COMMA + dst
	outp ostr

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' memory
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub hMemMoveRep _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval bytes as integer _
	) static

	dim as string dst, src
	dim as string ostr
	dim as integer ecxfree, edifree, esifree
	dim as integer ediinsrc, ecxinsrc

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( svreg, src )

	ecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )
	edifree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ESI )
	esifree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EDI )

	ediinsrc = hIsRegInVreg( svreg, EMIT_REG_EDI )
	ecxinsrc = hIsRegInVreg( svreg, EMIT_REG_ECX )

	if( ecxfree = FALSE ) then
		hPUSHAtt( "%ecx" )
	end if
	if( edifree = FALSE ) then
		hPUSHAtt( "%edi" )
	end if
	if( esifree = FALSE ) then
		hPUSHAtt( "%esi" )
	end if

	if( ediinsrc = FALSE ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hMOVAtt( "%edi", dst )
		else
			'' not esi already?
			if( dvreg->reg <> EMIT_REG_EDI ) then
           		hMOVAtt( "%edi", dst )
			end if
		end if

	else
		if( ecxinsrc ) then
			hPUSHAtt( "%ecx" )
		end if

		hMOVAtt( "%ecx", dst )

		if( ecxinsrc ) then
			outp "xchgl %ecx, (%esp)"
		end if
	end if

	if( svreg->typ <> IR_VREGTYPE_REG ) then
		hMOVAtt( "%esi", src )
	else
		'' not esi already?
		if( svreg->reg <> EMIT_REG_ESI ) then
           	hMOVAtt( "%esi", src )
		end if
	end if

	if( ediinsrc ) then
		if( ecxinsrc = FALSE ) then
			hMOVAtt( "%edi", "%ecx" )
		else
			hPOPAtt( "%edi" )
		end if
	end if

	if( bytes > 4 ) then
		ostr = "movl " + str( cunsg(bytes) \ 4 ) + ", %ecx"
		outp ostr
		outp "rep"
		outp "movsl"

	elseif( bytes = 4 ) then
		outp "mov (%esi), %ecx"
		outp "mov %ecx, (%edi)"
		if( (bytes and 3) > 0 ) then
			outp "addl $4, %esi"
			outp "addl $4, %edi"
		end if
	end if

		bytes and= 3
	if( bytes > 0 ) then
		if( bytes >= 2 ) then
			outp "movw (%esi), %cx"
			outp "movw %cx, (%edi)"
			if( bytes = 3 ) then
				outp "addl $2, %esi"
				outp "addl $2, %edi"
			end if
		end if

		if( (bytes and 1) <> 0 ) then
			outp "movb (%esi), %cl"
			outp "movb %cl, (%edi)"
		end if
	end if

	if( esifree = FALSE ) then
		hPOPAtt( "%esi" )
	end if
	if( edifree = FALSE ) then
		hPOPAtt( "%edi" )
	end if
	if( ecxfree = FALSE ) then
		hPOPAtt( "%ecx" )
	end if

end sub

'':::::
private sub hMemMoveBlk _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval bytes as integer _
	) static

	dim as string dst, src, aux
	dim as integer i, ofs, reg, isfree

	reg = hFindRegNotInVreg( dvreg )

	'' no free regs left?
	if( hIsRegInVreg( svreg, reg ) ) then
		hMemMoveRep( dvreg, svreg, bytes )
		exit sub
	end if

	aux = *hGetRegNameAtt( FB_DATATYPE_INTEGER, reg )

	isfree = hIsRegFree( FB_DATACLASS_INTEGER, reg )
	if( isfree = FALSE ) then
		hPUSHAtt( aux )
	end if

	ofs = 0
	'' move dwords
	for i = 1 to cunsg(bytes) \ 4
		hPrepOperandAtt( svreg, src, FB_DATATYPE_INTEGER, ofs )
		hMOVAtt( aux, src )
		hPrepOperandAtt( dvreg, dst, FB_DATATYPE_INTEGER, ofs )
		hMOVAtt( dst, aux )
		ofs += 4
	next

	'' a word left?
	if( (bytes and 2) <> 0 ) then
		aux = *hGetRegNameAtt( FB_DATATYPE_SHORT, reg )
		hPrepOperandAtt( svreg, src, FB_DATATYPE_SHORT, ofs )
		hMOVAtt( aux, src, FB_DATATYPE_SHORT )
		hPrepOperandAtt( dvreg, dst, FB_DATATYPE_SHORT, ofs )
		hMOVAtt( dst, aux, FB_DATATYPE_SHORT )
		ofs += 2
	end if

	'' a byte left?
	if( (bytes and 1) <> 0 ) then
		aux = *hGetRegNameAtt( FB_DATATYPE_BYTE, reg )
		hPrepOperandAtt( svreg, src, FB_DATATYPE_BYTE, ofs )
		hMOVAtt( aux, src, FB_DATATYPE_BYTE )
		hPrepOperandAtt( dvreg, dst, FB_DATATYPE_BYTE, ofs )
		hMOVAtt( dst, aux, FB_DATATYPE_BYTE )
	end if

	if( isfree = FALSE ) then
		hPOPAtt( aux )
	end if

end sub

'':::::
private sub _emitAttMEMMOVE _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval bytes as integer, _
		byval extra as integer _
	) static

	'' handle the assumption done at ast-node-mem::newMEM()
	if( bytes > EMIT_MEMBLOCK_MAXLEN ) then
		hMemMoveRep( dvreg, svreg, bytes )
	else
		hMemMoveBlk( dvreg, svreg, bytes )
	end if

end sub

'':::::
private sub _emitAttMEMSWAP _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval bytes as integer, _
		byval extra as integer _
	) static

	'' implemented as function

end sub

'':::::
private sub hMemClearRepIMM _
	( _
		byval dvreg as IRVREG ptr, _
		byval bytes as integer _
	) static

	dim as string dst
	dim as string ostr
	dim as integer eaxfree, ecxfree, edifree

	hPrepOperandAtt( dvreg, dst )

	eaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
	ecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )
	edifree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ESI )

	if( eaxfree = FALSE ) then
		hPUSHAtt( "%eax" )
	end if
	if( ecxfree = FALSE ) then
		hPUSHAtt( "%ecx" )
	end if
	if( edifree = FALSE ) then
		hPUSHAtt( "%edi" )
	end if

	if( dvreg->typ <> IR_VREGTYPE_REG ) then
		hMOVAtt( "%edi", dst )
	else
		'' not edi already?
		if( dvreg->reg <> EMIT_REG_EDI ) then
           	hMOVAtt( "%edi", dst )
        end if
	end if

	outp "xorl %eax, %eax"

	if( bytes > 4 ) then
		ostr = "movl $" + str( cunsg(bytes) \ 4 ) + ", %ecx"
		outp ostr
		outp "rep"
		outp "stosl"

	elseif( bytes = 4 ) then
		outp "movl %eax, (%edi)"
		if( (bytes and 3) > 0 ) then
			outp "addl $4, %edi"
		end if
	end if

	bytes and= 3
	if( bytes > 0 ) then
		if( bytes >= 2 ) then
			outp "movw %ax, (%edi)"
			if( bytes = 3 ) then
				outp "addl $2, %edi"
			end if
		end if

		if( (bytes and 1) <> 0 ) then
			outp "movb %al, (%edi)"
		end if
	end if

	if( edifree = FALSE ) then
		hPOPAtt( "%edi" )
	end if
	if( ecxfree = FALSE ) then
		hPOPAtt( "%ecx" )
	end if
	if( eaxfree = FALSE ) then
		hPOPAtt( "%eax" )
	end if

end sub

'':::::
private sub hMemClearBlkIMM _
	( _
		byval dvreg as IRVREG ptr, _
		byval bytes as integer _
	) static

	dim as string dst
	dim as integer i, ofs

	ofs = 0
	'' move dwords
	for i = 1 to cunsg(bytes) \ 4
		hPrepOperandAtt( dvreg, dst, FB_DATATYPE_INTEGER, ofs )
		hMOVAtt( dst, "$0" )
		ofs += 4
	next

	'' a word left?
	if( (bytes and 2) <> 0 ) then
		hPrepOperandAtt( dvreg, dst, FB_DATATYPE_SHORT, ofs )
		hMOVAtt( dst, "$0", FB_DATATYPE_SHORT )
		ofs += 2
	end if

	'' a byte left?
	if( (bytes and 1) <> 0 ) then
		hPrepOperandAtt( dvreg, dst, FB_DATATYPE_BYTE, ofs )
		hMOVAtt( dst, "$0", FB_DATATYPE_BYTE )
	end if

end sub

'':::::
private sub hMemClear _
	( _
		byval dvreg as IRVREG ptr, _
		byval bytes_vreg as IRVREG ptr _
	) static

	dim as string dst, bytes
	dim as string ostr
	dim as integer eaxfree, ecxfree, edifree

	hPrepOperandAtt( dvreg, dst )
	hPrepOperandAtt( bytes_vreg, bytes )

	eaxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_EAX )
	ecxfree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ECX )
	edifree = hIsRegFree( FB_DATACLASS_INTEGER, EMIT_REG_ESI )

	if( eaxfree = FALSE ) then
		hPUSHAtt( "%eax" )
	end if
	if( ecxfree = FALSE ) then
		hPUSHAtt( "%ecx" )
	end if
	if( edifree = FALSE ) then
		hPUSHAtt( "%edi" )
	end if

	if( hIsRegInVreg( bytes_vreg, EMIT_REG_EDI ) = FALSE ) then
		if( dvreg->typ <> IR_VREGTYPE_REG ) then
			hMOVAtt( "%edi", dst )
		else
			'' not edi already?
			if( dvreg->reg <> EMIT_REG_EDI ) then
            	hMOVAtt( "%edi", dst )
            end if
		end if

		if( bytes_vreg->typ <> IR_VREGTYPE_REG ) then
			hMOVAtt( "%ecx", bytes )
		else
			'' not ecx already?
			if( bytes_vreg->reg <> EMIT_REG_ECX ) then
            	hMOVAtt( "%ecx", bytes )
            end if
		end if

	else
		hPUSHAtt( bytes )

		ostr = "leal " + dst + ", %edi"
		outp ostr

		hPOPAtt( "%ecx" )
	end if

	outp "xorl %eax, %eax"

	outp "pushl %ecx"
	outp "shrl $2, %ecx"
	outp "rep"
	outp "stosl"
	outp "popl %ecx"
	outp "andl $3, %ecx"
	outp "rep"
	outp "stosb"

	if( edifree = FALSE ) then
		hPOPAtt( "%edi" )
	end if
	if( ecxfree = FALSE ) then
		hPOPAtt( "%ecx" )
	end if
	if( eaxfree = FALSE ) then
		hPOPAtt( "%eax" )
	end if

end sub

'':::::
private sub _emitAttMEMCLEAR _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval unused as integer, _
		byval extra as integer _
	)

	'' handle the assumption done at ast-node-mem::newMEM()
	if( irIsIMM( svreg ) ) then
		dim as integer bytes = svreg->value.int
		if( bytes > EMIT_MEMBLOCK_MAXLEN ) then
			hMemClearRepIMM( dvreg, bytes )
		else
			hMemClearBlkIMM( dvreg, bytes )
		end if

	else
    	hMemClear( dvreg, svreg )
	end if

end sub

'':::::
private sub _emitAttSTKCLEAR _
	( _
		byval dvreg as IRVREG ptr, _
		byval svreg as IRVREG ptr, _
		byval bytes as integer, _
		byval baseofs as integer _
	) static

	hClearLocalsAtt( bytes, baseofs )

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' debugging
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private sub _emitAttLINEINI _
	( _
		byval proc as FBSYMBOL ptr, _
		byval lnum as integer, _
		byval pos_ as integer _
	)

	edbgLineBegin( proc, lnum, pos_ )

end sub

'':::::
private sub _emitAttLINEEND _
	( _
		byval proc as FBSYMBOL ptr, _
		byval lnum as integer, _
		byval pos_ as integer _
	)

	edbgLineEnd( proc, lnum, pos_ )

end sub

'':::::
private sub _emitAttSCOPEINI _
	( _
		byval sym as FBSYMBOL ptr, _
		byval lnum as integer, _
		byval pos_ as integer _
	)

	edbgEmitScopeINI( sym )

end sub

'':::::
private sub _emitAttSCOPEEND _
	( _
		byval sym as FBSYMBOL ptr, _
		byval lnum as integer, _
		byval pos_ as integer _
	)

	edbgEmitScopeEND( sym )

end sub

''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' functions table
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#define EMIT_CBENTRY(op) @_emitAtt##op##

	'' same order as EMIT_NODEOP
	dim shared _opFnAttTB(0 to EMIT_MAXOPS-1) as any ptr => _
	{ _
		EMIT_CBENTRY(NOP), _
		_
		EMIT_CBENTRY(LOADI2I), EMIT_CBENTRY(LOADF2I), EMIT_CBENTRY(LOADL2I), _
		EMIT_CBENTRY(LOADI2F), EMIT_CBENTRY(LOADF2F), EMIT_CBENTRY(LOADL2F), _
		EMIT_CBENTRY(LOADI2L), EMIT_CBENTRY(LOADF2L), EMIT_CBENTRY(LOADL2L), _
        _
		EMIT_CBENTRY(STORI2I), EMIT_CBENTRY(STORF2I), EMIT_CBENTRY(STORL2I), _
		EMIT_CBENTRY(STORI2F), EMIT_CBENTRY(STORF2F), EMIT_CBENTRY(STORL2F), _
		EMIT_CBENTRY(STORI2L), EMIT_CBENTRY(STORF2L), EMIT_CBENTRY(STORL2L), _
        _
		EMIT_CBENTRY(MOVI), EMIT_CBENTRY(MOVF), EMIT_CBENTRY(MOVL), _
		EMIT_CBENTRY(ADDI), EMIT_CBENTRY(ADDF), EMIT_CBENTRY(ADDL), _
		EMIT_CBENTRY(SUBI), EMIT_CBENTRY(SUBF), EMIT_CBENTRY(SUBL), _
		EMIT_CBENTRY(MULI), EMIT_CBENTRY(MULF), EMIT_CBENTRY(MULL), EMIT_CBENTRY(SMULI), _
		EMIT_CBENTRY(DIVI), EMIT_CBENTRY(DIVF), NULL              , _
		EMIT_CBENTRY(MODI), NULL              , NULL              , _
		EMIT_CBENTRY(SHLI), EMIT_CBENTRY(SHLL), _
		EMIT_CBENTRY(SHRI), EMIT_CBENTRY(SHRL), _
		EMIT_CBENTRY(ANDI), EMIT_CBENTRY(ANDL), _
		EMIT_CBENTRY(ORI) , EMIT_CBENTRY(ORL) , _
		EMIT_CBENTRY(XORI), EMIT_CBENTRY(XORL), _
		EMIT_CBENTRY(EQVI), EMIT_CBENTRY(EQVL), _
		EMIT_CBENTRY(IMPI), EMIT_CBENTRY(IMPL), _
		EMIT_CBENTRY(ATN2), _
		EMIT_CBENTRY(POW), _
		EMIT_CBENTRY(ADDROF), _
		EMIT_CBENTRY(DEREF), _
        _
		EMIT_CBENTRY(CGTI), EMIT_CBENTRY(CGTF), EMIT_CBENTRY(CGTL), _
		EMIT_CBENTRY(CLTI), EMIT_CBENTRY(CLTF), EMIT_CBENTRY(CLTL), _
		EMIT_CBENTRY(CEQI), EMIT_CBENTRY(CEQF), EMIT_CBENTRY(CEQL), _
		EMIT_CBENTRY(CNEI), EMIT_CBENTRY(CNEF), EMIT_CBENTRY(CNEL), _
		EMIT_CBENTRY(CGEI), EMIT_CBENTRY(CGEF), EMIT_CBENTRY(CGEL), _
		EMIT_CBENTRY(CLEI), EMIT_CBENTRY(CLEF), EMIT_CBENTRY(CLEL), _
        _
		EMIT_CBENTRY(NEGI), EMIT_CBENTRY(NEGF), EMIT_CBENTRY(NEGL), _
		EMIT_CBENTRY(NOTI), EMIT_CBENTRY(NOTL), _
	   _
		NULL, _
        _
		EMIT_CBENTRY(ABSI), EMIT_CBENTRY(ABSF), EMIT_CBENTRY(ABSL), _
		EMIT_CBENTRY(SGNI), EMIT_CBENTRY(SGNF), EMIT_CBENTRY(SGNL), _
        _
		EMIT_CBENTRY(FIX), _
		EMIT_CBENTRY(FRAC), _
	   _
		NULL, _
	   _
		EMIT_CBENTRY(SIN), EMIT_CBENTRY(ASIN), _
		EMIT_CBENTRY(COS), EMIT_CBENTRY(ACOS), _
		EMIT_CBENTRY(TAN), EMIT_CBENTRY(ATAN), _
		EMIT_CBENTRY(SQRT), _
	   _
		NULL, _
		NULL, _
	   _
		EMIT_CBENTRY(LOG), _
		EMIT_CBENTRY(EXP), _
		EMIT_CBENTRY(FLOOR), _
		EMIT_CBENTRY(XCHGTOS), _
        _
		EMIT_CBENTRY(ALIGN), _
		EMIT_CBENTRY(STKALIGN), _
        _
		EMIT_CBENTRY(PUSHI), EMIT_CBENTRY(PUSHF), EMIT_CBENTRY(PUSHL), _
		EMIT_CBENTRY(POPI), EMIT_CBENTRY(POPF), EMIT_CBENTRY(POPL), _
		EMIT_CBENTRY(PUSHUDT), _
        _
		EMIT_CBENTRY(CALL), _
		EMIT_CBENTRY(CALLPTR), _
		EMIT_CBENTRY(BRANCH), _
		EMIT_CBENTRY(JUMP), _
		EMIT_CBENTRY(JUMPPTR), _
		EMIT_CBENTRY(RET), _
        _
		EMIT_CBENTRY(LABEL), _
		EMIT_CBENTRY(PUBLIC), _
		EMIT_CBENTRY(LIT), _
		EMIT_CBENTRY(JMPTB), _
        _
		EMIT_CBENTRY(MEMMOVE), _
		EMIT_CBENTRY(MEMSWAP), _
		EMIT_CBENTRY(MEMCLEAR), _
		EMIT_CBENTRY(STKCLEAR), _
        _
		EMIT_CBENTRY(LINEINI), _
		EMIT_CBENTRY(LINEEND), _
		EMIT_CBENTRY(SCOPEINI), _
		EMIT_CBENTRY(SCOPEEND) _
	}

'':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' emit.vtbl implementation
'':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
private function _open _
	( _
	) as integer

	if( hFileExists( env.outf.name ) ) then
		kill env.outf.name
	end if

	env.outf.num = freefile
	if( open( env.outf.name, for binary, access read write, as #env.outf.num ) <> 0 ) then
		return FALSE
	end if

	'' header
	hWriteHeader( )

	function = TRUE

end function

'':::::
private function _getVarName _
	( _
		byval s as FBSYMBOL ptr _
	) as string static

	dim as string sname

	if( s <> NULL ) then
		sname = *symbGetMangledName( s )

		if( symbGetOfs( s ) > 0 ) then
			sname += "+" + str( symbGetOfs( s ) )

		elseif( symbGetOfs( s ) < 0 ) then
			sname += str( symbGetOfs( s ) )
		end if

		function = sname

	else
		function = ""
	end if

end function

'':::::
private function _procGetFrameRegName _
	( _
	) as zstring ptr

	static as zstring * 4+1 sname = "%ebp"

	function = @sname

end function

'':::::
private sub _procFooter _
	( _
		byval proc as FBSYMBOL ptr, _
		byval bytestopop as integer, _
		byval initlabel as FBSYMBOL ptr, _
		byval exitlabel as FBSYMBOL ptr _
	)

    dim as integer oldpos = any, ispublic = any

    ispublic = symbIsPublic( proc )

	emitSection( IR_SECTION_CODE, 0 )

	''
	edbgEmitProcHeader( proc )

	''
	hALIGN( 16 )

	if( ispublic ) then
		hPUBLIC( symbGetMangledName( proc ), symbIsExport( proc ) )
	end if

	hLABEL( symbGetMangledName( proc ) )

	if( env.clopt.target = FB_COMPTARGET_LINUX ) then
		outEx( ".type " + *symbGetMangledName( proc ) + ", @function" + NEWLINE )
	end if

	'' frame
	hCreateFrameAtt( proc )

    edbgEmitLineFlush( proc, proc->proc.ext->dbg.iniline, proc )

    ''
    emitFlush( )

	''
	hDestroyFrameAtt( proc, bytestopop )

    edbgEmitLineFlush( proc, proc->proc.ext->dbg.endline, exitlabel )

    edbgEmitProcFooter( proc, initlabel, exitlabel )

end sub




''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' initialization/finalization
''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'':::::
function emitGasX86Att_ctor _
	( _
	) as integer
	
	emitGasX86_ctor '' super()

	emit.vtbl.open = @_open
	emit.vtbl.getVarName = @_getVarName
	emit.vtbl.procGetFrameRegName = @_procGetFrameRegName
	emit.vtbl.procFooter = @_procFooter
	
	emit.opFnTb = @_opFnAttTB(0)

	function = TRUE

end function
