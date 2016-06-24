unit Telegram.Plugin.WhoIs;

interface

uses
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
  End;

implementation

uses
  System.Net.WhoIs,
  System.SysUtils;

{ TTgWelcomeBot }

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
      TextExpr := WhoIs.WhoIs(TextExpr);
      if TextExpr.IndexOf('No match for') > -1 then
        TextExpr := 'Домен свободный!';
      (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
        TextExpr);
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
