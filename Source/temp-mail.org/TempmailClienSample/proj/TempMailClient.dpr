program TempMailClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  uGeneral in '..\UI\uGeneral.pas' {Form1},
  TempmailAPI in '..\..\Source\TempmailAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
