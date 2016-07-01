unit TelegAPI.Module.Calculator;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Utils,
  TelegAPI.Module;

Type
  TTgCalculatorBot = Class(TTgModule)
  private
    FIsCommandWait: Boolean;
  protected
    procedure OnUpdate(Sender: TObject; Const Update: TtgUpdate); override;
    Function MyKeyBoard: TtgInlineKeyboardMarkup;
  End;

implementation

uses
  TelegAPI.Controls,
  BI.Expression, System.SysUtils;

{ TTgWelcomeBot }

function TTgCalculatorBot.MyKeyBoard: TtgInlineKeyboardMarkup;
var
  CB: TtgInlineKeyboardButton;
begin
  Result := TtgInlineKeyboardMarkup.Create;
  CB := TtgInlineKeyboardButton.Create;
  CB.Text := 'sss';

  Result.inline_keyboard := [[CB]];
end;

procedure TTgCalculatorBot.OnUpdate(Sender: TObject; const Update: TtgUpdate);
var
  Cmd: TCommandHelper;
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
      (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
        TExpression.FromString(TextExpr).Value, TtgParseMode.Default, False,
        False, 0, nil);
    except
      on E: Exception do
        (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
          'упс, ошибочка вышла: ' + E.ClassName + ' ' + E.Message);
    end;
  End;

begin
  if NOT Assigned(Update.Message) then
    Exit;
  Cmd := TCommandHelper.Create(Update.Message.Text);
  try
    if Cmd.Command = '/calc' then
    Begin
      if Cmd.ParamCount = 0 then
      Begin
        FIsCommandWait := true;
        (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
          'ожидаю выражение:');
      End
      else
      Begin
        Calculation;
      End;
    End
    else if Cmd.Command = '/checkbox' then
    Begin
      (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID, 's',
        TtgParseMode.Default, true, False, 0, MyKeyBoard);
    end
    else if FIsCommandWait then
    Begin
      Calculation;
    End;
  finally
    Cmd.Free;
  end;

end;

end.
