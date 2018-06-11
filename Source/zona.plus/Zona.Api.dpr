program Zona.Api;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Net.HttpClient,
  System.SysUtils,
  ZonaAPI.Types in 'ZonaAPI.Types.pas',
  ZonaAPI in 'ZonaAPI.pas',
  ZonaAPI.FilterProcessor in 'ZonaAPI.FilterProcessor.pas';

procedure Test;
var
  LZona: TZonaAPI;
  LSer: TznCategory;
begin
  LZona := TZonaAPI.Create;
  try
    LSer := LZona.GetSeries;
    Writeln(LSer.title_h1);
  finally
    LZona.Free;
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

