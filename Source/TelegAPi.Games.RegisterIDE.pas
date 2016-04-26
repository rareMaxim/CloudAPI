unit TelegAPi.Games.RegisterIDE;

interface

Procedure Register;

implementation

uses
  uVictorine,
  TelegAPi.Games.Quitz,
  System.Classes;

Procedure Register;
Begin
  RegisterComponents('Telegram', [TVictorine, TTeleGameQuitz]);
End;

end.
