program PluginWhoIsDomain;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Telegram.Plugin.WhoIs in 'Telegram.Plugin.WhoIs.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
