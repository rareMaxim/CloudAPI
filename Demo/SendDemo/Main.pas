unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  TelegaPi.Exceptions,
  TelegAPI.Receiver.Base,
  TelegAPI.Receiver.Service,
  TelegAPI.Receiver.UI,
  TelegAPI.Base,
  TelegAPI.Bot.Impl,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  TelegAPi.Types,
  Vcl.Samples.Spin;

type
  TForm3 = class(TForm)
    pgc1: TPageControl;
    tsSettings: TTabSheet;
    grpToken: TGroupBox;
    btn1: TButton;
    grpChatID: TGroupBox;
    tsSendMessage: TTabSheet;
    TelegramBot1: TTelegramBot;
    tgReceiverUI1: TtgReceiverUI;
    tgExceptionManagerUI1: TtgExceptionManagerUI;
    tsExceptions: TTabSheet;
    edtToken: TEdit;
    edtChatID: TEdit;
    grpSendMsgText: TGroupBox;
    edtSendMsgText: TEdit;
    btnSendMsgText: TButton;
    rgParseMode: TRadioGroup;
    OpenDialog1: TOpenDialog;
    chkDisableWebPagePreview: TCheckBox;
    chkDisableNotification: TCheckBox;
    grpReplyToMsgID: TGroupBox;
    seReplyToMsgID: TSpinEdit;
    mmoInfo: TMemo;
    mmoExceptions: TMemo;
    procedure btn1Click(Sender: TObject);
    procedure btnSendMsgTextClick(Sender: TObject);
    procedure tgReceiverUI1Message(ASender: TObject; AMessage: ITgMessage);
    procedure tgExceptionManagerUI1ApiException(ASender: TObject; const AMethod: string; AApiRequestException: EApiRequestException);
    procedure tgExceptionManagerUI1GlobalException(ASender: TObject; const AMethod: string; AException: Exception);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  TelegaPi.Types.Enums;
{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
begin
  if tgReceiverUI1.IsActive then
  begin
    tgReceiverUI1.Stop;
  end;
  TelegramBot1.Token := edtToken.Text;
  tgReceiverUI1.Start;
end;

procedure TForm3.btnSendMsgTextClick(Sender: TObject);
begin
  TelegramBot1.SendMessage(//
    edtChatID.Text, //
    edtSendMsgText.Text, //
    TtgParseMode(rgParseMode.ItemIndex), //
    chkDisableWebPagePreview.Checked, //
    chkDisableNotification.Checked//
  );
end;

procedure TForm3.tgExceptionManagerUI1ApiException(ASender: TObject; const AMethod: string; AApiRequestException: EApiRequestException);
begin
  mmoExceptions.Lines.Add(Format('%S%S%S%S%S', [AMethod, slinebreak, AApiRequestException.ToString, slinebreak, '---------------------------------']));
  pgc1.ActivePage := tsExceptions;
end;

procedure TForm3.tgExceptionManagerUI1GlobalException(ASender: TObject; const AMethod: string; AException: Exception);
begin
  mmoExceptions.Lines.Add(Format('%S%S%S%S%S', [AMethod, slinebreak, AException.ToString, slinebreak, '---------------------------------']));
  pgc1.ActivePage := tsExceptions;
end;

procedure TForm3.tgReceiverUI1Message(ASender: TObject; AMessage: ITgMessage);
begin
  edtChatID.Text := AMessage.Chat.ID.ToString;
end;

end.

