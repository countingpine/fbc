' TEST_MODE : COMPILE_ONLY_OK

sub f( byval l as any ptr )
end sub

dim r as any const ptr = 0
f( r )
