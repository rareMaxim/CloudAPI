unit TelegAPi.RegisterIDE;

interface

procedure Register;

implementation

uses
  System.Classes,
  {Add new components here}

  TelegAPi.Bot;

procedure Register;
Begin
  RegisterComponents('Telegram', [TTelegramBot]);
End;

end.
