unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  TelegAPI.Base,
  TelegAPI.Bot.Impl,
  TelegAPi.Types,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.Layouts,
  TelegAPI.Receiver.Base,
  TelegAPI.Receiver.Service,
  TelegAPI.Receiver.UI;

type
  TForm2 = class(TForm)
    lyt1: TLayout;
    lblToken: TLabel;
    edtToken: TEdit;
    lytUser: TLayout;
    lblUser: TLabel;
    edtUser: TEdit;
    lytSendMessage: TLayout;
    lblSendMessage: TLabel;
    edtSendMessage: TEdit;
    btnSendMessage: TButton;
    TelegramBot1: TTelegramBot;
    tgReceiverUI1: TtgReceiverUI;
    btnApplyToken: TEditButton;
    lytPhoto: TLayout;
    lblPhoto: TLabel;
    edtPhoto: TEdit;
    btnPhoto: TButton;
    btnBrowse: TEditButton;
    dlgOpen1: TOpenDialog;
    procedure btnSendMessageClick(Sender: TObject);
    procedure btnApplyTokenClick(Sender: TObject);
    procedure tgReceiverUI1Message(ASender: TObject; AMessage: ITgMessage);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnPhotoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btnApplyTokenClick(Sender: TObject);
begin
  if tgReceiverUI1.IsActive then
  begin
    tgReceiverUI1.Stop;
  end;
  TelegramBot1.Token := edtToken.Text;
  tgReceiverUI1.Start;
end;

procedure TForm2.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    edtPhoto.Text := dlgOpen1.FileName;
end;

procedure TForm2.btnPhotoClick(Sender: TObject);
begin
  TelegramBot1.SendPhoto(edtUser.Text, TtgFileToSend.FromFile(edtPhoto.Text));
end;

procedure TForm2.btnSendMessageClick(Sender: TObject);
begin
  TelegramBot1.SendMessage(edtUser.Text, edtSendMessage.Text);
end;

procedure TForm2.tgReceiverUI1Message(ASender: TObject; AMessage: ITgMessage);
begin
  edtUser.Text := AMessage.Chat.ID.ToString;
end;

end.

