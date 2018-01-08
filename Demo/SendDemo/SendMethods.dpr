program SendMethods;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
