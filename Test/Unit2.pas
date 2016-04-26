unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  TelegAPI.Bot, TelegAPI.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, uVictorine, TelegAPI.Games.Quitz;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Victorine1: TVictorine;
    TeleGameQuitz1: TTeleGameQuitz;
    procedure TelegramBot1Update(const Sender: TObject; const Update: TTelegaUpdate);
    procedure TelegramBot1Error(Const Sender: TObject; Const Code: Integer; Const Message: String);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTelegramBot: TTelegramBot;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

  //
implementation

{$R *.fmx}

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  FTelegramBot.IsReceiving := CheckBox1.IsChecked;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FTelegramBot := TTelegramBot.Create(Self);
  FTelegramBot.Token := '122041176:AAG-1c6kALOL04CHpGWptk3dbvyoU3Njnds';
  FTelegramBot.OnUpdate := TelegramBot1Update;
  FTelegramBot.OnError := TelegramBot1Error;
  TeleGameQuitz1.TelegramBot := FTelegramBot;
  Victorine1.LoadFromFile('D:\VersionControl\DELPHI\atari\Game Files\Викторина.txt');
  CheckBox1.IsChecked := true;
end;

procedure TForm2.TelegramBot1Error(const Sender: TObject; const Code: Integer;
  const Message: String);
begin
  Memo1.BeginUpdate;
  try
    Memo1.Lines.Insert(0, string.Join(' ', ['[Error]', Code, Message]));
  finally
    Memo1.EndUpdate;
  end;
end;

procedure TForm2.TelegramBot1Update(const Sender: TObject; const Update: TTelegaUpdate);
var
  InputText: String;
  ku: Boolean;
begin
  //
  ku := False;
  Memo1.BeginUpdate;
  try
    if Memo1.Lines.Count > 100 then
      Memo1.Lines.Delete(Memo1.Lines.Count - 1);
    Memo1.Lines.Insert(0, Update.Message.From.Username + ' ' + Update.Message.Text);
    TeleGameQuitz1.ChatID := Update.Message.Chat.Id;
    InputText := Update.Message.Text.ToLower;
    if InputText.Contains('/find@test_delphi_api_bot') then

      if InputText.Contains('/go@test_delphi_api_bot') then
        // FTelegramBot.sendTextMessage(Update.Message.Chat.Id, Victorine1.RandomQuitz);
        TeleGameQuitz1.ShowQuitz
      else
        TeleGameQuitz1.ParseAnswer(InputText);
  finally
    Memo1.EndUpdate;
  end;

end;

end.
