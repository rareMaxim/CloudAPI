unit TelegAPi.RegisterIDE;

interface

procedure Register;

implementation

uses
  System.Classes,
  {Add new components here}
  TelegAPi.Bot,
  TelegAPi.Receiver.Service,
  TelegAPi.Receiver.UI;

{$IFDEF REG_IN_LOAD}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('CloudAPI', [TTelegramBot,  TtgReceiverService, TtgReceiverUI]);
end;

end.

