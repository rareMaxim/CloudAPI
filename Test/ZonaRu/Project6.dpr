program Project6;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  ZonaRu,
  ZonaRu.Types,
  System.SysUtils;

procedure Test;
var
  Zona: TZona;
  LMovie: IznCoverMedia;
begin
  Zona := TZona.Create(nil);
  try
    for LMovie in Zona.GetMovies(0,100) do
    begin
      Writeln(LMovie.name_rus,' ' , LMovie.abuse,' ', LMovie.id);
    end;
    Writeln(Zona.OpenMedia(LMovie.id.ToInt64).description);
  finally
    Zona.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Test;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
