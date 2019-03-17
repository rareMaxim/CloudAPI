program GoogleAnaliticsDemo;

uses
  Vcl.Forms,
  gAnalitics.Main in 'gAnalitics.Main.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
