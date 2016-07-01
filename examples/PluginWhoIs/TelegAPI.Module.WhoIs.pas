unit TelegAPI.Module.WhoIs;

interface

uses
  System.SysUtils,
  System.Net.WhoIs,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Utils,
  TelegAPI.Module;

Type
  TTgBotWhoIs = Class(TTgModule)
  private
    FIsCommandWait: Boolean;
  protected
    procedure OnUpdate(Sender: TObject; Const Update: TtgUpdate); override;
    function KeyBoard: TtgForceReply;
  End;

implementation

{ TTgWelcomeBot }

function TTgBotWhoIs.KeyBoard: TtgForceReply;
begin
  Result := TtgForceReply.Create;
  Result.force_reply := true;
  Result.selective := true;
end;

procedure TTgBotWhoIs.OnUpdate(Sender: TObject; const Update: TtgUpdate);
var
  Cmd: TCommandHelper;
  WhoIs: TWhoIs;
  Procedure Calculation;
  var
    TextExpr: String;
  Begin
    FIsCommandWait := False;
    if Cmd.ParamCount = 0 then
      TextExpr := Update.Message.Text
    else
      TextExpr := Cmd.ParamsToString;
    try
      TextExpr := TuaUtils.IfThen<String>(WhoIs.IsAvaible(TextExpr),
        '😍😍😍Домен ' + TextExpr + ' свободный!😍😍😍', '😡😡😡Домен ' + TextExpr +
        ' занятый😡😡😡');
      (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID, TextExpr,
        TtgParseMode.Html, True, False, 0, KeyBoard);
    except
      on E: Exception do
        (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
          'упс, ошибочка вышла: ' + E.ClassName + ' ' + E.Message);
    end;
  End;

begin
  Cmd := TCommandHelper.Create(Update.Message.Text);
  WhoIs := TWhoIs.Create;
  try
    if Cmd.Command = '/checkdomainname' then
    Begin
      if Cmd.ParamCount = 0 then
      Begin
        FIsCommandWait := true;
        (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
          'Ожидаю ввод доменов:');
      End
      else
      Begin
        Calculation;
      End;
    End
    else if Cmd.Command = '/knowndomain' then
      with (Sender as TTelegramBot) do
      Begin
        FIsCommandWait := False;
        sendTextMessage(Update.Message.Chat.ID, 'Поддерживаемые доменные зоны' +
          #13#10 + string.join(' ', WhoIs.WhoIsServers.Keys.ToArray));
      end
    else if Cmd.Command = '/finddomain' then
      with (Sender as TTelegramBot) do
      Begin
        FIsCommandWait := False;
        sendTextMessage(Update.Message.Chat.ID, 'Поддерживаемые доменные зоны' +
          #13#10 + string.join(' ', WhoIs.WhoIsServers.Keys.ToArray));
      end
    else if FIsCommandWait then
    Begin
      Calculation;
    End;
  finally
    Cmd.Free;
    WhoIs.Free;
  end;

end;

end.
