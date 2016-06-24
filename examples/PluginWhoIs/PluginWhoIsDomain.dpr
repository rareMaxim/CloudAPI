program PluginWhoIsDomain;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegaPi.Bot,
  TelegaPi.Types,
  System.SysUtils,
  Telegram.Plugin.WhoIs in 'Telegram.Plugin.WhoIs.pas',
  System.Net.WhoIs in 'System.Net.WhoIs.pas';

Const
  C_PAUSE_UPDATE = 1000; { 1sec. }

Var
  TelegramBot: TTelegramBot;
  TelegramWelcom: TTgBotWhoIs;

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
    Offset := Updates[High(Updates)].ID + 1;
  end;
End;

begin
  WriteLn('Telegram Calculator Sample');
  { Here you Api key }
  TelegramBot := TTelegramBot.Create({$I ..\telegaToken.inc} );
  TelegramWelcom := TTgBotWhoIs.Create(TelegramBot);
  try
    WriteLn('Bot token: ', TelegramBot.getMe.ID <> -1);
    InitRecesive;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  TelegramBot.Free;
  TelegramWelcom.Free;

end.
