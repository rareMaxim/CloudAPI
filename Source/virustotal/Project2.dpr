program Project2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  VirusTotal in 'VirusTotal.pas';

Var
  VT: TVirusTotalAPI;
  ResultScan: TvtURLReport;

begin
  VT := TVirusTotalAPI.Create;
  try
    { TODO -oUser -cConsole Main : Insert code here }
    ResultScan := VT.reportURL('https://codmasters.ru/');
    Writeln('Opera: ', ResultScan.scans.Opera.result);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  VT.Free;
  Readln;
end.
