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
  public
    { Public declarations }
    procedure OnGiveMsg(Sender: TObject; Updates: TArray<TtgUpdate>);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FBot := TTelegramBot.Create(Self);
  FBot.Token := {$I ..\token.inc};
  FBot.IsReceiving := True;
  FBot.OnUpdates := OnGiveMsg;
end;

procedure TForm1.OnGiveMsg(Sender: TObject; Updates: TArray<TtgUpdate>);
var
  I: Integer;
  LMessage: TtgMessage;
begin
  mmo1.BeginUpdate;
  try
    for I := Low(Updates) to High(Updates) do
    Begin
      if assigned(Updates[I].Message) then
      Begin
        mmo1.Lines.Add(Updates[I].Message.text);
        if Updates[I].Message.text = 'Hi' then
          LMessage := FBot.sendMessage(Updates[I].Message.Chat.ID, '😍😘😊😁😂😬😀😜😠😡');
        FreeAndNil(LMessage);
      End;
    End;
  finally
    mmo1.EndUpdate;
  end;
end;

end.
