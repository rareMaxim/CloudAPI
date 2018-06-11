program Project3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Mobizon in 'Mobizon.pas';

procedure Test;
var
  MZ: TmzMessage;
begin
  MZ := TmzMessage.Create;
  try
    MZ.Token := '';
    Writeln(MZ.SendSMSMessage('380684985731', 'Тест. Админцентр'));
    Readln;
  finally
    MZ.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Test;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

