program AutoRecesiveMessage;

uses
  System.StartUpCopy,
  FMX.Forms,
  AutoRecesiveMessage.Main in 'AutoRecesiveMessage.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
