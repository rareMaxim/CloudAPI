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
 // TelegAPi.Bot.Recesiver.UI,
 // TelegAPI.Bot.Recesiver.Console,
  TelegaPi.Exceptions,
  TelegAPi.Bot.Impl;

{$IFDEF REG_IN_LOAD}
{$ENDIF}

procedure Register;
begin
{$IFDEF REG_IN_LOAD}
  RegisterWithSplashScreen;
{$ENDIF}
  RegisterComponents('Telegram', [TTelegramBot, TtgExceptionManagerUI{, TTgBotRecesiverConsole, TTgBotRecesiverUI}]);
end;

end.

