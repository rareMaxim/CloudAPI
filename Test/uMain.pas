unit uMain;

interface

uses
  LoggerPro, TelegAPI.Bot, TelegAPI.Types,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    TelegramBot1: TTelegramBot;
    procedure FormCreate(Sender: TObject);
    procedure TelegramBot1Error(const Sender: TObject; const Code: Integer; const Message: string);
    procedure TelegramBot1Update(const Sender: TObject; const Update: TTelegaUpdate);
  private
    { Private declarations }
    FLog: ILogWriter;
  public
    { Public declarations }
    function Log: ILogWriter;
  end;

var
  Form2: TForm2;

implementation

uses
  LoggerPro.FMXMemoAppender;
{$R *.fmx}
{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  FLog := BuildLogWriter([TFMXMemoLogAppender.Create(Memo1)]);
  TelegramBot1.Token := {$I telegaToken.inc};
  TelegramBot1.IsReceiving := True;
end;

function TForm2.Log: ILogWriter;
begin
  Result := FLog;
end;

procedure TForm2.TelegramBot1Error(const Sender: TObject; const Code: Integer;
  const Message: string);
begin
  Log.Error(Message, Code.ToString);
end;

procedure TForm2.TelegramBot1Update(const Sender: TObject; const Update: TTelegaUpdate);
begin
  Log.Info(Update.Message.Text, Update.Message.From.Username);
end;

end.
