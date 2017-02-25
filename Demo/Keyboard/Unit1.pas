unit Unit1;

interface

uses
  Telegapi.Bot,
  Telegapi.Classes,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FBot: TTelegramBot;
    procedure OnGiveMsg(Sender: TObject; Updates: TArray<TtgUpdate>);
    procedure OnTgError(Sender: TObject; Const Code: Integer; Const Message: String);
  public
    { Public declarations }
    procedure DoSendKeyboard(Const ID: Int64);
    procedure DoSendEmoji(Const ID: Int64);

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoSendEmoji(const ID: Int64);
var
  LMessage: TtgMessage;
Begin
  LMessage := FBot.sendMessage(ID, '😍😘😊😁😂😬😀😜😠😡');
  FreeAndNil(LMessage);
end;

procedure TForm1.DoSendKeyboard(const ID: Int64);
var
  LMessage: TtgMessage;
  kb: TtgReplyKeyboardMarkup;
Begin
  kb := TtgReplyKeyboardMarkup.Create;
  try
    kb.one_time_keyboard := True; // Клавиатура скроется после нажатия на кнопку
    kb.RowCount := 2; // Кол-во рядов в клавиатуре
    kb.ColCount := 2; // Кол-во Столбцов в клавиатуре
    kb.KeyBoard[0] := [TtgKeyboardButton.Create('☺ Да'), TtgKeyboardButton.Create('😠 Нет')];
    kb.KeyBoard[1] := [TtgKeyboardButton.Create('❤️Красный'), TtgKeyboardButton.Create('💚 Зеленый')];
    LMessage := FBot.sendMessage(ID, '1', TtgParseMode.Default, False, False, 0, kb);
    FreeAndNil(LMessage);
  finally
    kb.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FBot := TTelegramBot.Create(Self);
  FBot.Token := {$I ..\token.inc};
  FBot.IsReceiving := True;
  FBot.OnUpdates := OnGiveMsg;
  FBot.OnError := OnTgError;
end;

procedure TForm1.OnGiveMsg(Sender: TObject; Updates: TArray<TtgUpdate>);
var
  I: Integer;
begin
  mmo1.BeginUpdate;
  try
    for I := Low(Updates) to High(Updates) do
    Begin
      if assigned(Updates[I].Message) then
      Begin
        mmo1.Lines.Add(Updates[I].Message.text);
        if Updates[I].Message.text = '😏' then
        Begin
          DoSendEmoji(Updates[I].Message.Chat.ID);
        End;
        if Updates[I].Message.text = 'key' then
        Begin
          DoSendKeyboard(Updates[I].Message.Chat.ID);
        End;
      End;
    End;
  finally
    mmo1.EndUpdate;
  end;
end;

procedure TForm1.OnTgError(Sender: TObject; const Code: Integer; const Message: String);
begin
  mmo1.Lines.Add('Error: ' + Code.ToString + ' ' + Message);
end;

end.
