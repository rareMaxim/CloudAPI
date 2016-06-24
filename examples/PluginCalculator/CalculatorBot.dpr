program CalculatorBot;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegaPi.Bot,
  TelegaPi.Types,
  System.SysUtils,
  Telegram.Plugin.Calculator in 'Telegram.Plugin.Calculator.pas';

Const
  C_PAUSE_UPDATE = 1000; { 1sec. }

Var
  TelegramBot: TTelegramBot;
  TelegramCalc: TTgCalculatorBot;

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
      Writeln(Update.Message.From.Username + ': ' + Update.Message.Text);
    end;
    Offset := Updates[High(Updates)].ID + 1;
  end;
End;

begin
  Writeln('Telegram Calculator Sample');
  { Here you Api key }
  TelegramBot := TTelegramBot.Create({$I ..\telegaToken.inc} );
  TelegramCalc := TTgCalculatorBot.Create(TelegramBot);
  try
    Writeln('Bot token: ', TelegramBot.getMe.ID <> -1);
    InitRecesive;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  TelegramBot.Free;
  TelegramCalc.Free;

end.
