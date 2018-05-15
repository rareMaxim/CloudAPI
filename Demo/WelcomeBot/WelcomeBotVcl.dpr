program WelcomeBotVcl;

uses
  Vcl.Forms,
  WB.MainVcl in 'WB.MainVcl.pas' {Form2},
  WelcomeBot.Config in 'WelcomeBot.Config.pas',
  WelcomeBot.Core in 'WelcomeBot.Core.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
