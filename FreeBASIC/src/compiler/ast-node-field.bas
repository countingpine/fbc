''	FreeBASIC - 32-bit BASIC Compiler.
''	Copyright (C) 2004-2008 The FreeBASIC development team.
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

'' AST variable nodes
'' l = NULL; r = NULL
''
'' chng: sep/2004 written [v1ctor]


#include once "inc\fb.bi"
#include once "inc\fbint.bi"
#include once "inc\ir.bi"
#include once "inc\ast.bi"

'':::::
function astNewFIELD _
	( _
		byval p as ASTNODE ptr, _
		byval sym as FBSYMBOL ptr, _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr = NULL _
	) as ASTNODE ptr

    dim as ASTNODE ptr n = any

	if( dtype = FB_DATATYPE_BITFIELD ) then
		select case symbGetType( subtype )
		case FB_DATATYPE_BOOL8, FB_DATATYPE_BOOL32
			'' final type is always a signed int
			dtype = typeJoin( dtype, FB_DATATYPE_INTEGER )
		case else
			'' final type is always an unsigned int
			dtype = typeJoin( dtype, FB_DATATYPE_UINT )
		end select
		subtype = NULL
	end if

	'' alloc new node
	n = astNewNode( AST_NODECLASS_FIELD, dtype, subtype )
	if( n = NULL ) then
		return NULL
	end if

	n->sym = sym
	n->l = p

	'' $$JRM
	'' convert to boolean if needed?
	if( ( dtype = FB_DATATYPE_BOOL8 ) or _
		( dtype = FB_DATATYPE_BOOL32 ) ) then
		n = astNewCONV( dtype, NULL, n )
	end if

	function = n

end function

'':::::
private function hGetBitField _
	( _
		byval n as ASTNODE ptr, _
		byval dtype as integer _
	) as ASTNODE ptr

	dim as ASTNODE ptr c = any
	dim as FBSYMBOL ptr s = any
	dim as integer boolconv = any

	s = n->subtype

	'' remap type - if boolean make sure the bool conversion is after 
	''              the bitfield access

	select case typeGet( s->typ )
	case FB_DATATYPE_BOOL8
		astGetFullType( n ) = typeJoin( astGetFullType( n ), FB_DATATYPE_BYTE )
		boolconv = TRUE
	case FB_DATATYPE_BOOL32
		astGetFullType( n ) = typeJoin( astGetFullType( n ), FB_DATATYPE_INTEGER )
		boolconv = TRUE
	case else
		astGetFullType( n ) = typeJoin( astGetFullType( n ), s->typ )
		boolconv = FALSE
	end select
	
	n->subtype = NULL

	'' make a copy, the node itself can't be used or it will be deleted twice
	c = astNewNode( INVALID, FB_DATATYPE_INVALID )
	astCopy( c, n )

	'' final type is always an integer (the sign depends if CONV changed
	'' the parent type or not)

	if( s->bitfld.bitpos > 0 ) then
		n = astNewBOP( AST_OP_SHR, c, _
				   	   astNewCONSTi( s->bitfld.bitpos, dtype ) )
	else
		n = c
	end if

	n = astNewBOP( AST_OP_AND, n, _
				   astNewCONSTi( ast_bitmaskTB(s->bitfld.bits), dtype ) )

	'' do boolean conversion after bitfield access
	if( boolconv ) then
		astGetFullType( n ) = typeJoin( astGetFullType( n ), s->typ )
		n = astNewCONV( FB_DATATYPE_INTEGER, NULL, n )
	end if

	function = n

end function

'':::::
sub astUpdateFieldAccess _ 
	( _ 
		byref n as ASTNODE ptr _
	)

	select case typeGetDtAndPtrOnly( astGetFullType( n ) )
	case FB_DATATYPE_BITFIELD
		n = hGetBitField( n, astGetFullType( n ) )
	case FB_DATATYPE_BOOL8, FB_DATATYPE_BOOL32
		n = astNewCONV( typeGetDtAndPtrOnly( astGetFullType( n ) ), NULL, n )
	end select
	
end sub

'':::::
private sub hWalk _
	( _
		byval node as ASTNODE ptr, _
		byval parent as ASTNODE ptr _
	)

    dim as ASTNODE ptr expr = any
    dim as FBSYMBOL ptr sym = any

	if( node->class = AST_NODECLASS_FIELD ) then
    	exit sub
    end if

	'' walk
	expr = node->l
	if( expr <> NULL ) then
		hWalk( expr, node )
	end if

	expr = node->r
	if( expr <> NULL ) then
		hWalk( expr, node )
	end if

end sub

'':::::
function astFieldUpdate _
	( _
		byval tree as ASTNODE ptr _
	) as ASTNODE ptr

    dim as ASTNODE ptr expr = any

	function = tree

    if( ast.typeinicnt <= 0 ) then
    	exit function
    end if

	'' walk
	expr = tree->l
	if( expr <> NULL ) then
		hWalk( expr, tree )
	end if

	expr = tree->r
	if( expr <> NULL ) then
		hWalk( expr, tree )
	end if

end function

'':::::
function astLoadFIELD _
	( _
		byval n as ASTNODE ptr _
	) as IRVREG ptr

    dim as ASTNODE ptr l = any

	'' handle special field access..
	l = n->l
	astUpdateFieldAccess( l )

	function = astLoad( l )

	astDelNode( l )

end function


