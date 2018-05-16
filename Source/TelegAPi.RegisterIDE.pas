unit TelegAPi.RegisterIDE;

interface

procedure Register;

implementation

{ /$DEFINE REG_IN_LOAD }

uses
{$IFDEF REG_IN_LOAD}
  VCL.Graphics,
  ToolsAPI,
{$ENDIF}
  System.Classes,
  {Add new components here}
  TelegAPi.Receiver.UI,
  TelegAPI.Logger.Old,
  TelegAPi.Bot.Impl,
  TelegAPi.Receiver.Service;

{$IFDEF REG_IN_LOAD}
{$ENDIF}

procedure Register;
begin
{$IFDEF REG_IN_LOAD}
  RegisterWithSplashScreen;
{$ENDIF}
  RegisterComponents('Telegram', [TTelegramBot, TtgExceptionManagerUI,
    TtgReceiverService, TtgReceiverUI]);
end;

end.

