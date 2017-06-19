program EchoBot;

uses
{$IFDEF  shadow_cs-leakcheck}
  LeakCheck,
{$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  EchoBot.Main in 'EchoBot.Main.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
