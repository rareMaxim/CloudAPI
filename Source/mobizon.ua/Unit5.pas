unit Unit5;

interface

uses
  Mobizon,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Clipbrd;

type
  TForm5 = class(TForm)
    tmr1: TTimer;
    grp1: TGroupBox;
    edt1: TEdit;
    cbb1: TComboBox;
    grp2: TGroupBox;
    grp3: TGroupBox;
    mmo1: TMemo;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FLastText: string;
    FClipBoard: TClipboard;
    procedure SendSMS(const Phone, Text: string);
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.btn1Click(Sender: TObject);
begin
  SendSMS(edt1.Text, mmo1.Text);
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  FClipBoard := TClipboard.Create;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  FClipBoard.Free;
end;

procedure TForm5.SendSMS(const Phone, Text: string);
var
  user: TmzUser;
  sms: TmzMessage;
begin
  user := TmzUser.Create;
  try
    user.Token := '';
    if user.GetOwnBalance.balance <= 0.5 then
    begin
      MessageDlg('Недостаточно средств на счету', TMsgDlgType.mtError, [mbOK], 0);
      Exit;
    end;
  finally
    user.Free;
  end;

  sms := TmzMessage.Create;
  try
    sms.Token := '';
    sms.SendSMSMessage(Phone, Text);
  finally
    sms.Free;
  end;
end;

procedure TForm5.tmr1Timer(Sender: TObject);
begin
  if FLastText <> FClipBoard.AsText then
  begin
    FLastText := FClipBoard.AsText;
    Application.Restore;
    Application.BringToFront;
    edt1.Text := FLastText;
  end;
end;

end.

