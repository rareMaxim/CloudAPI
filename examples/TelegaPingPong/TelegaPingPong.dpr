program TelegaPingPong;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegaPi.Bot,
  TelegaPi.Types,
  System.SysUtils;

Const
  C_PAUSE_UPDATE = 1000; { 1sec. }

Var
  TelegramBot: TTelegramBot;

Procedure Updatemanager(Const Update: TtgUpdate);
Begin
  if Update.Message.Text.ToLower.Contains('ping') then
    TelegramBot.sendTextMessage(Update.Message.Chat.ID, 'Pong')
End;

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
      Updatemanager(Update);
    Offset := Update.ID + 1;
  end;
End;

begin
  WriteLn('Telegram Bot Ping-Pong Sample');
  { Here you Api key }
  TelegramBot := TTelegramBot.Create({$I ..\telegaToken.inc} );
  try
    WriteLn('Bot token: ', TelegramBot.getMe.ID <> -1);
    InitRecesive;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  TelegramBot.Free;

end.
