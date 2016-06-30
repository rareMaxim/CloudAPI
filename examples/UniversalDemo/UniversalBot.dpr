program UniversalBot;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  XSuperObject,
  TelegaPi.Bot,
  TelegaPi.Types,
  System.SysUtils,
  TelegaPi.Module.Calculator
    in '..\PluginCalculator\TelegAPI.Module.Calculator.pas',
  System.Net.WhoIs in '..\PluginWhoIs\System.Net.WhoIs.pas',
  TelegaPi.Module.WhoIs in '..\PluginWhoIs\TelegAPI.Module.WhoIs.pas';

CONST
  C_PAUSE_UPDATE = 1000;

var
  TelegramBot: TTelegramBot;

Procedure InitRecesive;
var
  Offset: Integer;
  Updates: TArray<TtgUpdate>;
  Update: TtgUpdate;
Begin
  Offset := 0;
  while True do
  begin
    Sleep(C_PAUSE_UPDATE); // Update pause
    Updates := TelegramBot.getUpdates(Offset); // Get updates
    if Length(Updates) = 0 then
      Continue;
    for Update in Updates do
    begin
      Writeln(Update.AsJSONObject.AsJSON(True, True));
    end;
    Offset := Update.ID + 1;
  end;

End;

var
  TelegramCalc: TTgCalculatorBot;
  TelegramWhoIs: TTgBotWhoIs;

begin
  try
    Writeln('Telegram Universal Sample');
    { Here you Api key }
    TelegramBot := TTelegramBot.Create({$I ..\telegaToken.inc} );
    TelegramCalc := TTgCalculatorBot.Create(TelegramBot);
    TelegramWhoIs := TTgBotWhoIs.Create(TelegramBot);
    try
      Writeln('Bot Username: ', TelegramBot.getMe.Username);
      InitRecesive;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
    TelegramWhoIs.Free;
    TelegramCalc.Free;
    TelegramBot.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
