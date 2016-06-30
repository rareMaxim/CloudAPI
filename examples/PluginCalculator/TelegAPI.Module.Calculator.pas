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
    Function MyKeyBoard: TtgReplyKeyboardMarkup;
  End;

implementation

uses
  BI.Expression, System.SysUtils;

{ TTgWelcomeBot }

function TTgCalculatorBot.MyKeyBoard: TtgReplyKeyboardMarkup;
begin
  Result := TtgReplyKeyboardMarkup.Create;
  Result.one_time_keyboard := true;
  Result.KeyBoard := [[TtgKeyboardButton.Create('1'),
    TtgKeyboardButton.Create('2')]]
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
        False, 0, MyKeyBoard);
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
    else if FIsCommandWait then
    Begin
      Calculation;
    End;
  finally
    Cmd.Free;
  end;

end;

end.
