

{$IF CompilerVersion < 28}
	{$DEFINE USE_INDY}
{$ELSE}
	{$DEFINE USE_SYS_NET}
{$ENDIF}