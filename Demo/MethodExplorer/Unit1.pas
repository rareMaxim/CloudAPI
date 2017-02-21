unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, TelegAPI.Bot, TelegAPI.Classes,
  FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.ListBox;

type
  TForm1 = class(TForm)
    TelegramBot: TTelegramBot;
    mmo1: TMemo;
    vrtscrlbx1: TVertScrollBox;
    btnGetMe: TButton;
    btnSendMessage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TelegramBotUpdates(Sender: TObject; Updates: TArray<TelegAPI.Classes.TtgUpdate>);
    procedure btnGetMeClick(Sender: TObject);
    procedure btnSendMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnGetMeClick(Sender: TObject);
var
  LUser: TtgUser;
begin
  LUser := TelegramBot.getMe;
  mmo1.Lines.Add(Format('ID: %d, Username: %s ', [LUser.ID, LUser.Username]));
  LUser.Free;
end;

procedure TForm1.btnSendMessageClick(Sender: TObject);
begin
   TelegramBot.sendMessage()
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  TelegramBot.Token := {$I ..\token.inc};
  TelegramBot.IsReceiving := True;
end;

procedure TForm1.TelegramBotUpdates(Sender: TObject; Updates: TArray<TelegAPI.Classes.TtgUpdate>);
var
  I: Integer;
begin
  for I := Low(Updates) to High(Updates) do
    if Updates[I].Message <> nil then
    begin
      mmo1.Lines.Add(Updates[I].Message.text);
    end;
end;

end.
