unit Unit1;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Classes,
  TelegAPI.Utils,
  System.Threading,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FBot: TTelegramBot;
    Procedure OnError(Const Sender: TObject; Const Code: Integer;
      Const Message: String);
    Procedure OnUpdates(Sender: TObject; Const Updates: TArray<TtgUpdate>);
    Procedure UpdateManager(Const AUpdate: TtgUpdate);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FBot.IsReceiving := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FBot.IsReceiving := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FBot := TTelegramBot.Create(Self);
  FBot.OnError := OnError;
  FBot.OnUpdates := OnUpdates;

  FBot.Token := {$I ..\telegaToken.inc};
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBot.Free;
end;

procedure TForm1.OnError(const Sender: TObject; const Code: Integer;
  const Message: String);
begin
  Memo1.Lines.Add(string.Join(' ', ['Error', 'in', Sender.ClassName, 'Code =', Code,
    'Message =', Message]));
end;

procedure TForm1.OnUpdates(Sender: TObject; const Updates: TArray<TtgUpdate>);
begin
  TParallel.&For(low(Updates), High(Updates),
    Procedure(I: Integer)
    Begin
      UpdateManager(Updates[I]);
      Updates[I].Free;
    End);
end;

procedure TForm1.UpdateManager(const AUpdate: TtgUpdate);
var
  Cmd: TCommandHelper;
  LMessage: TtgMessage;
begin
  if NOT Assigned(AUpdate.Message) then
    Exit;
  Memo1.Lines.Add(AUpdate.Message.From.Username + ': ' + AUpdate.Message.Text);
  Cmd := TCommandHelper.Create(AUpdate.Message.Text);
  try
    if Cmd.Command = '/ping' then
      LMessage := FBot.sendTextMessage(AUpdate.Message.Chat.ID, '👑🙈😇');
  finally
    Cmd.Free;
    LMessage.Free;
  end;
end;

end.
