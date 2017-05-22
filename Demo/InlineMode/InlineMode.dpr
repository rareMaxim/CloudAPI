program InlineMode;

uses
  System.StartUpCopy,
  FMX.Forms,
  InlineMode.Main in 'InlineMode.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
